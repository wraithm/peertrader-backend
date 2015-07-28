{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Route.StreamData where

import           Snap.Core
import           Snap.Extras.JSON       (writeJSON)

import           Control.Monad.IO.Class (liftIO)

import           Data.Aeson
import           Data.Maybe             (fromJust)
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Data.Vector            as V

import           Application
import           Prosper
import           Prosper.Monad

-- | Requesting streaming data
-- The url is formed like: /streamdata?channel=numls
-- where numls is the name of the data stream
--
-- TODO Throw a 404 if there's no data at the requested site
handleStreamData :: AppHandler ()
handleStreamData = do
--     with sess touchSession -- TODO Doesn't work! Not implemented in the interface!
    p <- getParam "channel"
    maybe noChannel handleChannel p
  where
    noChannel = writeLBS "Error: No data" -- TODO Throw 404

    handleChannel "totalamtrem" =
        handleTimeSeries (V.sum . V.map amountRemaining)
    handleChannel "numlistings" = handleTimeSeries V.length
    handleChannel "avgfico" =
        handleTimeSeries (remainingWeightedAvg (fromIntegral . fico . credit))
    handleChannel "revolvingbalance" =
        handleTimeSeries (remainingWeightedAvg (fromJust . revolvingBalance . credit))
    handleChannel "revolvingcredit" =
        handleTimeSeries (remainingWeightedAvg (fromIntegral . fromJust . revolvingAvailableCredit . credit))
    handleChannel "monthsemployed" =
        handleTimeSeries (remainingWeightedAvg (fromIntegral . fromJust . monthsEmployed . credit))
    handleChannel "amountdelinquent" =
        handleTimeSeries (remainingWeightedAvg (fromJust . amountDelinquent . credit))
    handleChannel "opencreditlines" =
        handleTimeSeries (remainingWeightedAvg (fromIntegral . fromJust . openCreditLines . credit))
    handleChannel _ = noChannel

-- mean :: Vector Double -> Double
-- mean = uncurry (/) . V.foldl' (\(s, c) e -> (e + s, c + 1)) (0,0)

handleTimeSeries :: ToJSON a => (Vector Listing -> a) -> AppHandler ()
handleTimeSeries f = do
    x <- handleProsper $ do
            lss <- getAllListings
            return $ f lss
    t <- liftIO getTime
    writeJSON (t, x)

getTime :: IO Int
getTime = (* 1000) . truncate <$> getPOSIXTime

remainingWeightedAvg :: (Listing -> Double) -> Vector Listing -> Double
remainingWeightedAvg f ls = V.sum (V.zipWith (*) weights (V.map f ls)) / V.sum weights
  where
    weights :: Vector Double
    weights = V.map amountRemaining ls
