{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module PeerTrader.Investment.Database where

import           Control.Monad.IO.Class     (MonadIO, liftIO)

import           Data.Time.Clock            (UTCTime, getCurrentTime)

import           Database.Groundhog
import           Database.Groundhog.TH

import           Prosper                    (InvestResponse (..), InvestMessage (..),
                                             InvestStatus (..))

import           PeerTrader.StrategyType
import           PeerTrader.Types           (UserLogin)

data Investment = Investment
    { owner        :: !UserLogin
    , timeStamp    :: !UTCTime
    , strategyType :: !StrategyType
    , investment   :: !InvestResponse
    } deriving Show

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - primitive: InvestMessage
  - primitive: InvestStatus
  - embedded: InvestResponse
  - entity: Investment
    constructors:
      - name: Investment
        fields:
          - name: owner
            type: text
            reference:
              table: snap_auth_user
              columns: [login]
          - name: timeStamp
            type: timestamptz
            default: now()
|]

insertResponse :: (PersistBackend m, MonadIO m, Functor m)
    => UserLogin
    -> StrategyType
    -> InvestResponse
    -> m ()
insertResponse n t investResp = do
    now <- liftIO getCurrentTime
    insert_ $ Investment n now t investResp

userResponses :: (PersistBackend m, MonadIO m)
    => UserLogin
    -> m [Investment]
userResponses n = select $ OwnerField ==. n
