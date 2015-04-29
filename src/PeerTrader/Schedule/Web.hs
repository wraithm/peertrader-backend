{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Schedule.Web where

import           Control.Concurrent          (threadDelay)
import           Control.Monad.Logger
import           Control.Monad.Reader

import           Data.Attoparsec.Text        (parseOnly)
import           Data.Time.Clock             (getCurrentTime)

import           System.Cron
import           System.Cron.Parser          (cronSchedule)

import           Logging                     (debugM)

import           Application
import           PeerTrader.Admin.Statistics
import           PeerTrader.Prosper.Account

statSchedule :: CronSchedule
statSchedule =
    let Right s = parseOnly cronSchedule "* 0,3,7,9,15,19,12,11,21,23 * * *"
    in s

scheduleLoop :: App -> IO ()
scheduleLoop app = runNoLoggingT $ flip runReaderT app $ forever $ do
    now <- liftIO getCurrentTime
    when (scheduleMatches statSchedule now) $ do
        saveAccounts
        updateStats
    debugM "Schedule" "Sleeping..."
    liftIO $ threadDelay oneHour
  where
    oneHour = 3600000000
