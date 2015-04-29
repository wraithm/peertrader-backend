{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module P2PPicks.Types where

import           Data.Aeson
import           Data.Serialize

import           Database.Groundhog    ()
import           Database.Groundhog.TH

import           GHC.Generics

data P2PPicksType
    = ProfitMax
    | LossMin
    deriving (Show, Eq, Read, Generic)

instance ToJSON P2PPicksType where
instance FromJSON P2PPicksType where

instance Serialize P2PPicksType where

mkPersist defaultCodegenConfig [groundhog|
primitive: P2PPicksType
|]
