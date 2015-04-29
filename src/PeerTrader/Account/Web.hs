{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module PeerTrader.Account.Web where

import           Control.Concurrent                 (ThreadId)
import           Control.Concurrent.STM
import           Control.Lens

import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           Prosper

import           PeerTrader.P2PPicks.Account

data AccountData = AccountData
    { _prosperUser          :: User
    , _prosperAccount       :: TVar Account
    , _prosperAccountThread :: Maybe ThreadId
    , _prosperResponses     :: TChan InvestResponse

    , _p2ppicksAccount      :: Maybe P2PPicksAccount
    }

makeLenses ''AccountData

instance Show AccountData where
    show (AccountData ui _ att _ _) = "AccountData " ++ show ui ++ " " ++ show att

instance ToRow AccountData where
    toRow (AccountData { _prosperUser = User {..} }) =
        [ toField username
        , toField password
        ]

accountData :: User -> Account -> Maybe P2PPicksAccount -> IO AccountData
accountData ui a p2ppicks = do
    (ta, tr) <- atomically $ do
        ta <- newTVar a
        tr <- newTChan
        return (ta, tr)
    return $ AccountData ui ta Nothing tr p2ppicks
