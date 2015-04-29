{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module PeerTrader.P2PPicks.Result where

import           Control.Monad.IO.Class (liftIO)

import           Data.Time.Clock        (UTCTime, getCurrentTime)

import           Database.Groundhog
import           Database.Groundhog.TH

import           P2PPicks

import           PeerTrader.Database    (runDb)
import           PeerTrader.Ops

data P2PPicksResult = P2PPicksResult
    { p2pListingId :: !Int
    , p2pTimeStamp :: !UTCTime
    , p2pPicksType :: !P2PPicksType -- ^ PMax | LMin
    , p2pChoice    :: !Bool -- ^ True if p2p-picks did select this one to invest
    } deriving (Show, Eq)

mkPersist defaultCodegenConfig [groundhog|
entity: P2PPicksResult
constructors:
  - name: P2PPicksResult
    fields:
      - name: p2pTimeStamp
        type: timestamptz
        default: now()
|]

insertP2PPicksResult
    :: Int -- ^ Listing ID
    -> P2PPicksType
    -> Bool -- ^ Did invest
    -> OpsReader ()
insertP2PPicksResult lid p2pType didInvest = do
   now <- liftIO getCurrentTime
   runDb $ insert_ $ P2PPicksResult lid now p2pType didInvest

didP2PPicksInvest :: PersistBackend m => Int -> P2PPicksType -> m Bool
didP2PPicksInvest lid p2pType = do
    results <- select $
        P2pListingIdField ==. lid &&.
        P2pPicksTypeField ==. p2pType
    case results of
        P2PPicksResult _ _ _ choice:_ -> return choice
        _ -> return False
