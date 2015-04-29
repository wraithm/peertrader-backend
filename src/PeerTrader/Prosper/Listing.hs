{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module PeerTrader.Prosper.Listing where

import           Control.Concurrent.STM
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger   (runNoLoggingT)
import           Control.Monad.Reader   (runReaderT)

import           Data.Time.Clock        (UTCTime, getCurrentTime)

import           Database.Groundhog     (AutoKey, insert)
import           Database.Groundhog.TH

import qualified Prosper.Listing        as P
import           Prosper.Monad          (prosperProcessingQueue)

import           PeerTrader.Database    (runDb)
import           PeerTrader.Ops

data ListingResult = ListingResult
    { timeStamp :: !UTCTime
    , listingId :: !Int
    }

insertListing :: P.Listing -> OpsReader (AutoKey ListingResult)
insertListing l = runDb $ do
    now <- liftIO getCurrentTime
    insert $ ListingResult now lid
  where lid = P.listingId l

listingResultLoop :: Ops -> IO ()
listingResultLoop ops = do
    pq <- atomically $ dupTChan (prosperProcessingQueue (_prosperState ops))
    forever $ do
        l <- atomically $ readTChan pq
        runOpsDbReader (insertListing l)
  where runOpsDbReader = runNoLoggingT . flip runReaderT ops

mkPersist defaultCodegenConfig [groundhog|
entity: ListingResult
constructors:
  - name: ListingResult
    fields:
      - name: timeStamp
        type: timestamptz
        default: now()
|]
