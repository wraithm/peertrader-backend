{-# LANGUAGE RecordWildCards #-}

module PeerTrader.P2PPicks where

import           Control.Concurrent.STM     (atomically, dupTChan, readTChan,
                                             readTVarIO, writeTChan)
import           Control.Monad              (forever, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Logger       (runNoLoggingT)
import           Control.Monad.Reader       (runReaderT)

import           Data.Functor               ((<$>))
import           Data.Maybe                 (isJust)

import           System.IO.Streams          as Streams
import           System.IO.Streams.Util

import           P2PPicks
import           Prosper
import           Prosper.Commands           (listingCSV)
import           Prosper.MarketDataUser
import           Prosper.Monad

import           PeerTrader.Ops
import           PeerTrader.P2PPicks.Result

p2pPicksLoop :: Ops -> IO ()
p2pPicksLoop ops = do
    -- Get User of market data account
    mdAcct <- readTVarIO (prosperMarketDataAccount ps)
    let ui = mdUser mdAcct

    -- Duplicate new listings queue
    pq <- atomically $ dupTChan (prosperProcessingQueue ps)

    -- Get new listings, get CSV data, feed it to guardian,
    -- and then users
    forever $ do
        l <- atomically $ readTChan pq
        runProsper ps $ liftIO $ listingCSV ui l (handleCsvStream l)
  where
    ps = _prosperState ops
    pmaxResults = _p2pProfitMaxResults ops
    lminResults = _p2pLossMinResults ops
    P2PPicks{..} = _p2ppicks ops

    opsDb = runNoLoggingT . flip runReaderT ops

    handleCsvStream l csvStream = do
        -- Start guardians, one for ProfitMax, one for LossMin
        (inPMax, outPMax, phPMax) <- runGuardian profitMaxLicense
        (inLMin, outLMin, phLMin) <- runGuardian lossMinLicense

        -- Feed csv data to guardians
        bifurcate csvStream inPMax inLMin

        -- Get results from guardian
        investPMax <- isJust <$> Streams.read outPMax
        investLMin <- isJust <$> Streams.read outLMin

        -- Close guardians
        _ <- waitForProcess phPMax
        _ <- waitForProcess phLMin

        -- Write results to DB, TODO Maybe write after feeding to strategies
        let lid = listingId l
        opsDb $ insertP2PPicksResult lid ProfitMax investPMax
        opsDb $ insertP2PPicksResult lid LossMin investLMin

        -- Feed Listings to p2p-picks strategies
        atomically $ do
            when investPMax $ writeTChan pmaxResults l
            when investLMin $ writeTChan lminResults l

    runGuardian picksType = do
        (inp, out, _, h) <- Streams.runInteractiveProcess guardian
            [ "/dev/stdin"
            , picksType
            , paramM, paramB
            ] Nothing Nothing
        return (inp, out, h)
