{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PeerTrader.Route.User where

import           Control.Applicative               ((<|>))
import           Control.Concurrent                (forkIO)
import           Control.Concurrent.Async
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Representable.State (get)
import           Control.Monad.Trans.Reader

import           Data.Aeson
import qualified Data.ByteString.Lazy              as L
import           Data.Monoid                       ((<>))
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T

import           Heist
import qualified Heist.Interpreted                 as I
import           Snap
import           Snap.Extras.JSON                  (writeJSON)
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.SES

import qualified Text.Blaze.Html5                  as H
import qualified Text.Blaze.Html5.Attributes       as A
import           Text.Blaze.Internal               (textValue)

import           Application
import qualified PeerTrader.Account.Handler        as A
import           PeerTrader.NewUser
import           PeerTrader.Types                  (UserLogin)

-- | Retrieve and Submit the password reset form
handleChangePassword :: AuthAppHandler ()
handleChangePassword =
    method GET handleGET <|>
    method POST handlePOST
  where
    handleGET  = currentUser >>= maybe the403 (\_ -> render "changepassword")
    handlePOST = currentUser >>= maybe the403 handleUser
    handleUser user@AuthUser{..} = do
        Just password <- getParam "password"
        _ <- saveUser =<< liftIO (setPassword user password)
        _ <- clearPasswordResetToken userLogin
        redirect "/"

the403 :: Handler App a ()
the403 = do
    modifyResponse $ setResponseCode 403
    render "403"

the404 :: Handler App a ()
the404 = do
    modifyResponse $ setResponseCode 404
    writeBS "404"

sendForgotPasswordEmail :: AppHandler ()
sendForgotPasswordEmail =
    method POST handlePOST <|>
    method GET (render "forgot")
  where
    handlePOST = do
        Just userLogin <- fmap T.decodeUtf8 <$> getParam "login"
        Just url       <- fmap T.decodeUtf8 <$> getParam "url"
        handleURL userLogin url =<< with auth (setPasswordResetToken userLogin)

    handleURL _ _ Nothing = render "usernotfound"
    handleURL userLogin url (Just token) = do
        let userEmail = L.fromStrict $ T.encodeUtf8 userLogin
            resetUrl  = url <> "/resetPassword?code=" <> token <> "&email=" <> userLogin
        keys <- with awsKeys get
        _ <- liftIO $ forkIO $ do
            let mail = flip runReaderT keys $
                    sendEmailBlaze [userEmail] subject (body resetUrl)
            void $ waitCatch =<< async mail -- TODO log error here
        render "sentResetEmail"

    subject = "PeerTrader Password Recovery"
    body resetUrl =
        H.html $
            H.body $ do
            H.h3 "To reset your password, follow the link and enter your new password:"
            H.a H.! A.href (textValue resetUrl) $ "Click here to reset your password"
            H.br
            H.p "If you did not request a password change you can safely ignore this email"
            H.p "Best regards,"
            H.p "PeerTrader team support@peertrader.com"

handleResetPassword :: AppHandler ()
handleResetPassword = method GET $ do
    Just token <- fmap T.decodeUtf8 <$> getParam "code"
    Just login <- fmap T.decodeUtf8 <$> getParam "email"
    authMgr <- with auth get
    result <- liftIO $ lookupByLogin authMgr login
    case result of
        Nothing -> the404
        Just user@AuthUser{..} ->
            if userResetToken /= Just token
                then the404
                else do
                    _ <- with auth $ forceLogin user
                    redirect "/changepassword"

-- | Render login form
handleLogin :: Maybe T.Text -> AuthAppHandler ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err

-- | Handle Login Submit
handleLoginSubmit :: AuthAppHandler ()
handleLoginSubmit = method GET handleGET <|> method POST handlePOST
  where
    handleGET = currentUser >>= maybe (render "login") (const $ redirect "/")
    handlePOST = loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
    err = Just "Unknown user or password"

-- | Logs out and redirects the user to the site index.
-- TODO remove account data streaming
handleLogout :: AuthAppHandler ()
handleLogout = logout >> redirect "/"

-- | Handle new user form submit
handleNewUser :: AppHandler ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = with auth $ render "new_user"
    handleFormSubmit = do
        -- TODO Pattern match correctly
        Just login <- fmap T.decodeUtf8 <$> getParam "login"
        Just password <- getParam "password"
        -- TODO Validate state isn't empty
        Just state <- fmap T.decodeUtf8 <$> getParam "state"

        newUser login password state >>= whenJust (sendVerifyEmail login)
        render "/verifyemailsent"
    whenJust = maybe (return ())

-- | Helper for 'handleNewUser'
-- Sends an email to a new user with a verification code.
sendVerifyEmail :: UserLogin -> T.Text -> AppHandler ()
sendVerifyEmail login verifyCode = do
    Just url <- fmap T.decodeUtf8 <$> getParam "url"
    let verifyUrl = url
            <> "/verifynewuser?verifycode=" <> verifyCode
            <> "&email=" <> login
    keys <- with awsKeys get
    void $ liftIO $ forkIO $ do
        let mail = flip runReaderT keys $
                sendEmailBlaze [userEmail] subject (body verifyUrl)
        void $ waitCatch =<< async mail -- TODO Log error
  where
    subject = "PeerTrader Registration Verification"
    userEmail = L.fromStrict $ T.encodeUtf8 login

    body verifyUrl = H.html $ H.body $ do
        H.h3 "To verify and login to your new PeerTrader account, follow this link: "
        H.a H.! A.href (textValue verifyUrl) $ "Click here to verify your account"
        H.br
        H.p "If you did not register with PeerTrader, you can safely ignore this email."
        H.p "Best regards,"
        H.p "PeerTrader team support@peertrader.com"

-- | Test if verifycode is equal to what's in the database.
-- Create a new user if they are verified.
handleVerify :: AppHandler ()
handleVerify = method GET $ do
    Just code <- fmap T.decodeUtf8 <$> getParam "verifycode"
    Just login <- fmap T.decodeUtf8 <$> getParam "email"
    verifyUser login code

-- | Check if the user needs to check the terms
-- and conditions of PeerTrader. If so, redirect them
-- to the terms page.
handleCheckTerms :: AppHandler ()
handleCheckTerms = method GET getCheckTerms <|> method POST setCheckTerms
  where
    setCheckTerms = withCurrentUser $ \n -> do
        -- TODO Just pattern
        Just checkTerm <- getParam "checkTerms"
        A.setCheckTerms n (readCheckTerm checkTerm)

    readCheckTerm "false" = False
    readCheckTerm "true" = True
    readCheckTerm _ = True

    getCheckTerms = withCurrentUser_ $ \n -> do
        Just terms <- A.getCheckTerms n
        writeJSON $ object [ "checkTerms" .= terms ]

-- | This handler requires the user to be logged in.
-- There is no database cost to this proceedure. This only requires
-- that the username be in the session.
-- TODO Perhaps should throw a 401 Unauthorized here.
requireLogIn :: AppHandler () -> AppHandler ()
requireLogIn = requireUser auth whenNotLoggedIn
  where whenNotLoggedIn = writeLBS "Error: Required to log in!"
