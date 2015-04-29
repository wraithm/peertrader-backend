{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module PeerTrader.Admin.Database where

import           Data.Aeson
import           Data.Time.Clock       (UTCTime)

import           GHC.Generics

import           Database.Groundhog    ()
import           Database.Groundhog.TH

import           NoteScript            (Money)

data Statistics = Statistics
    { timeStamp           :: !UTCTime
    , strategiesActive    :: !Int
    , strategies          :: !Int
    , totalAccountValue   :: !Money
    , totalAmountInvested :: !Money
    } deriving (Generic, Show)

instance ToJSON Statistics where
instance FromJSON Statistics where

mkPersist defaultCodegenConfig [groundhog|
entity: Statistics
constructors:
  - name: Statistics
    fields:
      - name: timeStamp
        type: timestamptz
        default: now()
|]
