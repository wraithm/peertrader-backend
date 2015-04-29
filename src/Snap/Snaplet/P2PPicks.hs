{-# LANGUAGE OverloadedStrings     #-}

module Snap.Snaplet.P2PPicks
    ( p2ppicksInit
    , validateUser
    , subscriberStatus
    , reportInvestment
    , SubscriberID
    , P2PPicksKeys (..)
    ) where

import           Snap.Snaplet

import           Control.Monad.IO.Class (liftIO)

import           P2PPicks.Keys

p2ppicksInit :: SnapletInit b P2PPicksKeys
p2ppicksInit = makeSnaplet "p2ppicks" "P2PPicksKeys" Nothing $ do
    config <- getSnapletUserConfig
    liftIO $ initializeP2PPicksKeys config
