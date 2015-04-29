{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Schedule.Ops where

import           Control.Concurrent           (threadDelay)
import           Control.Monad.Logger
import           Control.Monad.Reader

import           Data.Attoparsec.Text         (parseOnly)
import           Data.Time.Clock              (getCurrentTime)

import           System.Cron
import           System.Cron.Parser           (cronSchedule)

import           Logging                      (debugM)

import           PeerTrader.Ops
import           PeerTrader.Strategy.Schedule

investStateSchedule :: CronSchedule
investStateSchedule =
    let Right s = parseOnly cronSchedule "* 0 * * *"
    in s

scheduleLoop :: Ops -> IO ()
scheduleLoop ops = runNoLoggingT $ flip runReaderT ops $ forever $ do
    now <- liftIO getCurrentTime
    when (scheduleMatches investStateSchedule now) updateDailyInvested
    debugM "Schedule" "Sleeping..."
    liftIO $ threadDelay oneHour
  where
    oneHour = 3600000000
