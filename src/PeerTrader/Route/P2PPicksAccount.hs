{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Route.P2PPicksAccount
    ( p2paccountHandler
    ) where

import           Snap.Core
import           Snap.Extras.CoreUtils                        (badReq)
import           Snap.Snaplet                                 (with)
import           Snap.Snaplet.P2PPicks

import           Control.Applicative                          ((<|>))
import           Control.Exception                            (SomeException)
import           Control.Monad                                (join)
import           Control.Monad.CatchIO                        (catch)
import           Control.Monad.Util                           (ifM, whenM)

import           Data.ByteString

import           Database.Groundhog

import           Logging                                      (debugM)

import           Application
import           PeerTrader.Account.Web                       (p2ppicksAccount)
import           PeerTrader.P2PPicks.Account
import           PeerTrader.Socket.Web                        (deactivateStrategy)
import           PeerTrader.StrategyManagement.ActiveStrategy
import           PeerTrader.Types                             (UserLogin)

p2paccountHandler :: AppHandler ()
p2paccountHandler =
    method GET accountUserName <|>
    method POST updateP2PAccount <|>
    method DELETE deleteP2PAccount

-- TODO FIX THIS... Don't just respond with a bad pattern match...
accountUserName :: AppHandler ()
accountUserName = withCurrentUser_ $ \n -> do
    Just (P2PPicksAccount _ un _ _ _) <-
        join <$> getAccountVar n p2ppicksAccount
    writeBS un

-- | Get the username and password info. Save the data to memory,
-- and also save the data to disk.
updateP2PAccount :: AppHandler ()
updateP2PAccount = do
    uname <- getParam "username"
    pwd <- getParam "password"

    let ui = (,) <$> uname <*> pwd

    withCurrentUser_ $ \n -> ifAccount n $
        maybe (badAccount uname pwd) (addAccount n) ui
  where
    ifAccount n k = ifM (existsAccount n) k noProsperAccount
    noProsperAccount = badReq "Could not find Prosper Account."

    badAccount Nothing Nothing =
        badReq "Neither password nor username were provided"
    badAccount Nothing _ = badReq "Username was not provided"
    badAccount _ Nothing = badReq "Password was not provided"
    badAccount _ _ = badReq "Something went horribly wrong."

deleteP2PAccount :: AppHandler ()
deleteP2PAccount = withCurrentUser_ $ \n -> do
    putAccountVar n p2ppicksAccount Nothing
    runGH $ delete $ P2ppicksOwnerField ==. n
    whenM (isP2PPicksStratActive n) (deactivateStrategy n)
  where
    isP2PPicksStratActive n = runGH $ do
        p2pStrats <- select $ p2pOwnerField ==. n
        return (not $ Prelude.null p2pStrats)


addAccount :: UserLogin -> (ByteString, ByteString) -> AppHandler ()
addAccount n (username, password) = checkAccount $ \(sid, isActive) -> do
    let p2ppicksAcct = P2PPicksAccount n username password sid isActive
    putAccountVar n p2ppicksAccount (Just p2ppicksAcct)
    runGH $ insert_ p2ppicksAcct
    debugM "P2PPicks" $ "Account " ++ show (n, sid, isActive)
  where
    -- Before creating an account, check if there is actually data,
    -- if there is data, then continue, else noProsperAccount
    checkAccount k =
        (with p2ppicks (validateUser username password) >>=
        maybe noP2PAccount k)
      `catch`
        noP2PAccountE

    noP2PAccount :: AppHandler ()
    noP2PAccount = badReq "Could not find P2P-Picks Account."

    noP2PAccountE :: SomeException -> AppHandler ()
    noP2PAccountE e = do
        debugM "P2PPicks" $ "Check account exception! " ++ show e
        noP2PAccount
