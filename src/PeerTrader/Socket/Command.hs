{-# LANGUAGE DeriveGeneric #-}

module PeerTrader.Socket.Command where

import           GHC.Generics

import           Data.ByteString     (ByteString)
import           Data.Serialize
import           Data.Serialize.Text ()

import           Prosper.Invest

import           PeerTrader.Types

data Command
    = ActivateAutoFilter UserLogin Int -- ^ User, DefaultKey (Strategy AutoFilter)
    | ActivateP2PPicks UserLogin Int -- ^ User, DefaultKey (Strategy P2PPicksType)
    | Deactivate UserLogin
    | NewProsperAccount UserLogin ByteString ByteString -- ^ User, Username, Password
    | DeleteProsperAccount UserLogin
    deriving (Generic, Show)

instance Serialize Command where

data Response = Response UserLogin InvestResponse
    deriving (Generic, Show)

instance Serialize Response where
