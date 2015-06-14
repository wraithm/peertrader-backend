{-# LANGUAGE OverloadedStrings #-}

-- | Handle prosper account data.
--
-- 'acountHandler' sends account data to the client and also updates the database
-- with Prosper data.
--
-- 'prosperAccountController' checks on the status of various functions, including
-- account data polling and also the NoteScript strategy. POSTing to this handler
-- will activate the account data polling.
module PeerTrader.Route.ProsperAccount
    ( accountHandler
    , accountDataHandler
    , prosperAccountController
    ) where

import           Snap.Core
import           Snap.Extras.CoreUtils         (badReq, notFound)
import           Snap.Extras.JSON              (writeJSON)

import           Control.Applicative           ((<$>), (<*>), (<|>))
import           Control.Exception             (SomeException)
import           Control.Monad                 (join)
import           Control.Monad.CatchIO         (Handler (..), catches)
import           Control.Monad.IO.Class        (liftIO)

import           Data.Aeson                    (object, (.=))
import           Data.Maybe                    (isJust)
import           Data.Text
-- import           Data.Traversable              as T

import           Logging
import           Prosper

import           Application
-- import           PeerTrader                    (killProsper)
import           PeerTrader.Account.Handler    (deleteAccount,
                                                setEnabledProsperAccount,
                                                updateAccount)
import           PeerTrader.Account.Web
import           PeerTrader.Socket.Web         (deleteProsperUser,
                                                newProsperUser)
import           PeerTrader.Strategy.Strategy
import           PeerTrader.StrategyManagement (currentInvestState,
                                                currentStrategy)
import           PeerTrader.Types              (UserLogin)

prosperAccountController :: AppHandler ()
prosperAccountController =
    method GET prosperAccountStatus <|>
    method POST activateProsperAccount <|>
    method DELETE deactivateProsperAccount

-- | This handler checks on the status of both the account data thread and
-- the NoteScript strategy thread. This sends json of the form:
--
-- { "accountStatus": true|false,
--   "scriptStatus": true|false,
--   "scriptName": null|"name",
--   "scriptAmount": null|12.32 }
prosperAccountStatus :: AppHandler ()
prosperAccountStatus = withCurrentUser_ $ \n -> do
    actt <- isJust . join <$> getAccountVar n prosperAccountThread
    mAf <- runGH $ currentStrategy n
    let scriptName = name <$> mAf
        scriptAmount = amount <$> mAf
        scriptRisk = riskSettings <$> mAf

--     TODO GET THESE FROM THE OPS SERVER
--     st <- isJust . join <$> getAccountVar n prosperScriptThread
--     tInvestState <- join <$> getAccountVar n prosperInvestState
--     investState <- T.mapM (liftIO . readTVarIO) tInvestState
--     TESTING THIS IS JUST FROM DB
    let st = isJust mAf
    investState <- runGH $ currentInvestState n

    writeJSON $ object
        [ "accountStatus" .= actt
        , "scriptStatus" .= st -- TODO GET FROM OPS
        , "scriptName" .= scriptName
        , "scriptAmount" .= scriptAmount
        , "scriptRisk" .= scriptRisk
        , "scriptInvestState" .= investState -- TODO GET FROM OPS
        ]

-- | Activate the account data polling thread.
activateProsperAccount :: AppHandler ()
activateProsperAccount = withCurrentUser_ $ \n -> do
    debugM "Accounts" $ unpack n ++ " just activated their account"
    initAccountUpdate n tenSeconds
    prosperAccountStatus
  where
    tenSeconds = 10000000

-- | Deactivate the account data polling thread.
deactivateProsperAccount :: AppHandler ()
deactivateProsperAccount = withCurrentUser_ $ \n -> do
    debugM "Accounts" $ unpack n ++ " just deactivated their account"
    killAccountUpdate n
    prosperAccountStatus

-- | This sends "null" when there is an error.
-- Send json of the prosper account data.
accountDataHandler :: AppHandler ()
accountDataHandler = withCurrentUser getProsperAccount >>= writeJSON

accountHandler :: AppHandler ()
accountHandler =
    method GET accountUserName <|>
    method POST updateProsperAccount <|>
    method DELETE deleteProsperAccount

accountUserName :: AppHandler ()
accountUserName = withCurrentUser_ $ \n -> do
    Just (User { username = un }) <- getAccountVar n prosperUser
    writeBS un

-- | Get the username and password info. Save the data to memory,
-- and also save the data to disk.
updateProsperAccount :: AppHandler ()
updateProsperAccount = do
    uname <- getParam "username"
    pwd <- getParam "password"

    let ui = User <$> uname <*> pwd

    withCurrentUser_ $ \n ->
        maybe (badAccount uname pwd) (addAccount n) ui
  where
    badAccount Nothing Nothing = badReq "Neither password nor username were provided"
    badAccount Nothing _ = badReq "Username was not provided"
    badAccount _ Nothing = badReq "Password was not provided"
    badAccount _ _ = badReq "Something went horribly wrong."

deleteProsperAccount :: AppHandler ()
deleteProsperAccount = withCurrentUser_ $ \n -> do
    killAccountUpdate n
    deleteProsperUser n
    deleteAccount n
    setEnabledProsperAccount n False
    removeAccount n

addAccount :: UserLogin -> User -> AppHandler ()
addAccount n ui = checkAccount $ \a -> do
    acct <- liftIO $ accountData ui a Nothing
    updateAccount n acct
    setEnabledProsperAccount n True
    putAccount n acct
    newProsperUser n ui
    -- debugM "Accounts" . show  =<< liftIO . readTVarIO =<< use accounts
  where
    -- Before creating an account, check if there is actually data,
    -- if there is data, then continue, else noProsperAccount
    checkAccount k =
            (liftIO (account ui) >>= k)
        `catches`
            [ Handler noMoneyInAccount
            , Handler noProsperAccount
            ]

    noMoneyInAccount :: AccountException -> AppHandler ()
    noMoneyInAccount (AccountException msg) = do
        debugM "ProsperAccount" $ "500 error, probably no money in account: " ++ show msg
        badReq "Prosper account has no money. Please fill account."

    noProsperAccount :: SomeException -> AppHandler ()
    noProsperAccount e = do
        debugM "ProsperAccount" $ "CheckAccount caught exception: " ++ show e
        notFound "Could not find Prosper Account."
