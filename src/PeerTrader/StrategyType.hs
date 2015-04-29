{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module PeerTrader.StrategyType where

import           Data.Serialize

import           Database.Groundhog    ()
import           Database.Groundhog.TH

import           GHC.Generics

import           P2PPicks.Types

data StrategyType
    = AutoFilterStrategy
    | P2PPicksStrategy P2PPicksType
    deriving (Generic, Show, Read, Eq)

instance Serialize StrategyType where

mkPersist defaultCodegenConfig [groundhog|
primitive: StrategyType
|]
