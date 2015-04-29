{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.StrategyManagement.SelectStrategy where

import           Data.Aeson
import           Data.Text                        (Text)

import           Database.Groundhog
import           Database.Groundhog.Core

import           GHC.Generics

import           NoteScript (Money)
import           P2PPicks.Types                   (P2PPicksType)

import           PeerTrader.AutoFilter.AutoFilter
import           PeerTrader.Database
import           PeerTrader.Strategy.Strategy
import           PeerTrader.Types                 (UserLogin)

-- | For the frontend.
data SelectStrategy
    = AutoFilterStrategy
        { strategyId     :: !Int
        , strategyName   :: !Text
        , strategyAmount :: !Money
        }
    | P2PPicksStrategy
        { strategyId     :: !Int
        , strategyName   :: !Text
        , strategyAmount :: !Money
        }
    deriving (Show, Eq, Generic)

instance ToJSON SelectStrategy where
instance FromJSON SelectStrategy where

-- TODO Eventually, this will need to be abstracted. I just don't want to send
-- the whole filter over to the frontend.
userStrategies :: PersistBackend m => UserLogin -> m [SelectStrategy]
userStrategies n = do
    afs <- selectEntity StrategyConstructor $ afOwnerStrat ==. n
    p2ps <- selectEntity StrategyConstructor $ p2pOwnerStrat ==. n
    return $ map afsToFrontend afs ++ map p2psToFrontend p2ps
  where
    afOwnerStrat :: Field (Strategy AutoFilter) StrategyConstructor UserLogin
    afOwnerStrat = OwnerField

    p2pOwnerStrat :: Field (Strategy P2PPicksType) StrategyConstructor UserLogin
    p2pOwnerStrat = OwnerField

    afsToFrontend :: Entity (Strategy a) -> SelectStrategy
    afsToFrontend (Entity k (Strategy nm _ a _ _)) =
        AutoFilterStrategy k nm a

    p2psToFrontend :: Entity (Strategy a) -> SelectStrategy
    p2psToFrontend (Entity k (Strategy nm _ a _ _)) =
        P2PPicksStrategy k nm a
