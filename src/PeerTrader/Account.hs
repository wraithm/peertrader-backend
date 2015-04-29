{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module PeerTrader.Account where

import           Control.Concurrent           (ThreadId)
import           Control.Concurrent.STM
import           Control.Lens

import           Prosper

import           PeerTrader.Database
import           PeerTrader.Strategy.Strategy

data PeerTraderAccount = PTA
    { _prosperUser         :: User
    , _prosperChans        :: ProsperChans
      -- ^ If a user has a strategy active, then activate chans

    , _prosperInvestState  :: Maybe (TVar (Entity InvestState))

    , _prosperInvestThread :: Maybe ThreadId
    , _prosperScriptThread :: Maybe (ThreadId, ThreadId)
    }

data ProsperChans = ProsperChans
    { -- | Need to use dupTChan of the broadcast chan from ProsperState
      _readListings :: Maybe (TChan Listing)

    , _investWrite  :: TChan InvestResponse -- ^ Write end
    , _investRead   :: TChan InvestResponse -- ^ Read end
    }

makeLenses ''PeerTraderAccount
makeLenses ''ProsperChans

instance Show PeerTraderAccount where
    show (PTA ui _ _ it st) =
        "PeerTraderAccount " ++
        show ui ++ " " ++
        show it ++ " " ++
        show st
