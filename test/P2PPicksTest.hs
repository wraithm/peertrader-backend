{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Configurator   as C
import           Data.Vector         as V

import           NoteScript
import           P2PPicks
import           Prosper

import           PeerTrader
import           PeerTrader.P2PPicks

main :: IO ()
main = do
    ps <- initializePeerTrader
    p2ppicksConfig <- load [ Optional "snaplets/p2ppicks/devel.cfg" ]
    p2p <- initializeP2PPicks "snaplets/p2ppicks" p2ppicksConfig

    ui <- runProsper ps (fmap mdUserInfo getMarketDataUserInfo)
    listings <- runProsper ps $
        requestListings . mdUserInfo =<< getMarketDataUserInfo

    let script = do
            pmax <- p2ppicksStrategy p2p ProfitMax
            lmin <- p2ppicksStrategy p2p LossMin
            return (pmax, lmin)
        runStrat l = do
            result <- runProsper ps (prosperScript ui l script)
            debugM "Test" $ show (listingId l, result)

    V.mapM_ runStrat listings
