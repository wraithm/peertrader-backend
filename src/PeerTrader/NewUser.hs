{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module PeerTrader.NewUser where

import           Prelude                    hiding (null)

import           Snap                       (redirect)
import           Snap.Extras                (finishEarly)
import           Snap.Snaplet               (with, withTop')
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session       (randomToken)

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State        (gets)

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C
import           Data.Maybe                 (listToMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, null, strip)
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Time.Clock            (UTCTime, getCurrentTime)

import           Database.Groundhog         (delete, insert_, select, (==.))
import           Database.Groundhog.TH

import           Application                (AppHandler, AuthAppHandler, auth,
                                             runGH)
import qualified PeerTrader.Account.Handler as A
import           PeerTrader.Types           (UserLogin)

data NewUser = NewUser
    { newUserLogin      :: !UserLogin
    , newUserPassword   :: !ByteString -- ^ This must be the encrypted password
    , newUserVerifyCode :: !Text
    , newUserState      :: !Text
    , newUserCreatedAt  :: !UTCTime
    }

mkPersist defaultCodegenConfig [groundhog|
entity: NewUser
constructors:
  - name: NewUser
    fields:
      - name: newUserCreatedAt
        type: timestamptz
        default: now()
|]

insertNewUser :: UserLogin -> ByteString -> Text -> Text -> AppHandler ()
insertNewUser login pass verifyCode state = do
    now <- liftIO getCurrentTime
    runGH $ insert_ $ NewUser login pass verifyCode state now

lookupNewUser :: UserLogin -> AppHandler (Maybe NewUser)
lookupNewUser login = runGH $ do
    newuser <- select $ NewUserLoginField ==. login
    return (listToMaybe newuser)

deleteNewUser :: UserLogin -> AppHandler ()
deleteNewUser login = runGH $
    delete $ NewUserLoginField ==. login

newUser :: UserLogin -> ByteString -> Text -> AppHandler (Maybe Text)
newUser login pass state =
    lookupNewUser login >>= maybe createNewUser existsNewUser
  where
    existsNewUser _ = return Nothing
    createNewUser = do
        rng <- with auth $ gets randomNumberGenerator
        verifyCode <- liftIO $ decodeUtf8 <$> randomToken 40 rng
        -- Encrypt the password to store in the db
        encPass <- liftIO (encrypt pass)
        insertNewUser login encPass verifyCode state
        return (Just verifyCode)

verifyUser :: UserLogin -> Text -> AppHandler ()
verifyUser ulogin userCode =
    lookupNewUser ulogin >>=
    maybe noRequestedUser checkVerifyCode
  where
    -- TODO check error code
    noRequestedUser = finishEarly 401 "New user not found"
    -- TODO Handle bad registration
    couldNotCreate e = finishEarly 501 $
        "Could not create auth user: " <> C.pack (show e)

    -- | Ripped off from 'createUser' in 'Snap.Snaplet.Auth'
    -- I made this function so that I could create a user
    -- from an encrypted password.
    createUserEncrypted
        :: UserLogin
        -> ByteString
        -> AuthAppHandler (Either AuthFailure AuthUser)
    createUserEncrypted login encPass
        | null $ strip login = return $ Left UsernameMissing
        | otherwise = do
            uExists <- usernameExists login
            if uExists
                then return $ Left DuplicateLogin
                else withBackend $ \r -> liftIO $ do
                    now <- getCurrentTime
                    let au = defAuthUser
                            { userLogin = login
                            , userPassword = Just (Encrypted encPass)
                            , userCreatedAt = Just now
                            , userUpdatedAt = Just now
                            }
                    save r au

    checkVerifyCode (NewUser login pass verifyCode state _)
        | verifyCode == userCode = with auth $
            createUserEncrypted login pass >>= either couldNotCreate (\authUser -> do
                withTop' id $ do
                    -- TODO Handle bad database insert
                    A.newPeerTraderAccount login state
                    deleteNewUser login
                _ <- forceLogin authUser
                redirect "/")
        -- TODO Figure out error code
        | otherwise = finishEarly 401 "Bad verification code"
