{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Route.Statistics where

import           Snap.Core
import           Snap.Extras                   (writeJSON)

import           Data.Aeson                    (object, (.=))

import           Application                   (AppHandler, runGH,
                                                withCurrentUser)
import           PeerTrader.Account.Statistics
import           PeerTrader.Prosper.Account    (accountTimeSeries)

statisticsHandler :: AppHandler ()
statisticsHandler = method GET $ do
    r <- hitRate
    s <- successRate
    writeJSON $ object
        [ "hitRate" .= r
        , "successRate" .= s
        ]

accountTimeSeriesHandler :: AppHandler ()
accountTimeSeriesHandler = method GET $
    withCurrentUser (runGH . accountTimeSeries) >>= writeJSON
