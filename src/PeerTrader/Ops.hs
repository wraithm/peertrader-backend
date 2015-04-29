{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module PeerTrader.Ops where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Reader

import           Data.HashMap.Strict           as H
import           Data.Monoid                   (First)
import           Data.Pool
import           Data.Traversable              as T

import           Database.Groundhog.Core       hiding (get)
import           Database.Groundhog.Postgresql hiding (get)

import           P2PPicks
import           P2PPicks.Keys
import           Prosper
import           Prosper.Monad

import           PeerTrader.Account
import           PeerTrader.Types

data Ops = Ops
    { _prosperState        :: ProsperState
    , _accounts            :: TVar (HashMap UserLogin PeerTraderAccount)
    , _ghPool              :: Pool Postgresql

    -- P2P Picks resources
    , _p2ppicks            :: P2PPicks
    , _p2ppicksKeys        :: P2PPicksKeys

    , _p2pProfitMaxResults :: TChan Listing
    , _p2pLossMinResults   :: TChan Listing
    }

makeLenses ''Ops

type OpsReader = ReaderT Ops (NoLoggingT IO)

instance ConnectionManager Ops Postgresql where
    withConn f ops = withConn f (_ghPool ops)
    withConnNoTransaction f ops = withConnNoTransaction f (_ghPool ops)

getAccount :: UserLogin -> OpsReader (Maybe PeerTraderAccount)
getAccount n = do
    accts <- liftIO . readTVarIO =<< asks _accounts
    return $ accts ^. at n

putAccount :: UserLogin -> PeerTraderAccount -> OpsReader ()
putAccount n acct = do
    t <- asks _accounts
    liftIO . atomically $ modifyTVar' t (H.insert n acct)

removeAccount :: UserLogin -> OpsReader ()
removeAccount n = do
    t <- asks _accounts
    liftIO . atomically $ modifyTVar' t (H.delete n)

getAccountVar :: UserLogin -> Getting (First a) PeerTraderAccount a -> OpsReader (Maybe a)
getAccountVar n l = do
    acct <- getAccount n
    return $ acct ^? _Just.l

putAccountVar :: UserLogin -> Setter' PeerTraderAccount a -> a -> OpsReader ()
putAccountVar n l x = void $ do
    acct <- getAccount n
    let acct' = set (_Just.l) x acct
    T.mapM (putAccount n) acct'

-- TODO Abstract out reading account lens stuff
updateUserVar :: UserLogin -> Getter PeerTraderAccount (TVar a) -> a -> OpsReader ()
updateUserVar n l x = do
    t <- getAccountVar n l
    maybe (return ()) (\t' -> liftIO . atomically $ writeTVar t' x) t
    -- TODO Report error if this fails

readUserVar :: UserLogin -> Getter PeerTraderAccount (TVar a) -> OpsReader (Maybe a)
readUserVar n l = do
    t <- getAccountVar n l
    T.mapM (liftIO . readTVarIO) t
    -- TODO Report error if this fails

evalProsper :: Prosper a -> OpsReader a
evalProsper p = do
    ps <- asks _prosperState
    liftIO $ runProsper ps p
