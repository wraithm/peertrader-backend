{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent                           (forkIO)
import           Control.Concurrent.STM
import           Control.Monad                                (foldM, forever,
                                                               (<=<))
import           Control.Monad.IO.Class                       (liftIO)
import           Control.Monad.Reader                         (runReaderT)

import           Data.Configurator                            as C
import           Data.HashMap.Strict                          as H
import           Data.Pool

import qualified Database.Groundhog                           as GH
import           Database.Groundhog.Postgresql
import           Database.Groundhog.Utils.Postgresql          (keyToInt)

import           Logging
import           NoteScript                                   (prosperScript)
import           P2PPicks                                     hiding (reportInvestment)
import           P2PPicks.Keys
import           Prosper
import           Prosper.Monad

import           PeerTrader
import           PeerTrader.Account
import qualified PeerTrader.Account.Account                   as A
import           PeerTrader.AutoFilter.AutoFilter
import           PeerTrader.Database
import           PeerTrader.Investment.Database
import           PeerTrader.Ops
import           PeerTrader.P2PPicks
import           PeerTrader.P2PPicks.Account
import           PeerTrader.Prosper.Listing
import           PeerTrader.Schedule.Ops
import           PeerTrader.Socket.Ops
import           PeerTrader.Strategy.Strategy
import           PeerTrader.StrategyManagement.ActiveStrategy
import           PeerTrader.StrategyType
import           PeerTrader.Types

initializeDatabase
    :: Pool Postgresql
    -> ProsperState
    -> P2PPicksKeys
    -> TChan Listing -- ProfitMax Results
    -> TChan Listing -- LossMin Results
    -> IO (TVar (HashMap UserLogin PeerTraderAccount))
initializeDatabase g ps p2pKeys pmaxResults lminResults = do
    res <- runDbConn selectUserInfo g
    hm <- foldM newAccount H.empty res
    newTVarIO hm
  where
    selectUserInfo = select $ A.ProsperEnabledField ==. True

    maybe' :: Maybe a -> b -> (a -> b) -> b
    maybe' m d f = maybe d f m

    newAccount
        :: HashMap UserLogin PeerTraderAccount
        -> A.PeerTraderAccount
        -> IO (HashMap UserLogin PeerTraderAccount)
    newAccount m (A.PeerTraderAccount login _ (Just userinfoKey) _ _) = do
        mUserinfo <- flip runDbConn g $ get userinfoKey
        maybe' mUserinfo (return m) $ \userinfo -> do
            -- Where investment TChans, write and read ends
            (i, r, isr) <- atomically $ do
                i <- newBroadcastTChan
                r <- dupTChan i
                isr <- dupTChan i
                return (i, r, isr)

            -- Initialize strategy if active
            (mAf, mP2P) <- flip runDbConn g $ do
                -- Get the active strategy and state
                let getStratState ownerField = do
                        strats <- project ActiveStrategyField $ ownerField ==. login
                        case strats of
                            stratKey:_ -> do
                                mStrat <- get stratKey
                                mState <- getInvestState stratKey
                                case (mStrat, mState) of
                                    (Just strat, Just state) -> return $ Just (strat, state)
                                    (Just strat, _) -> do
                                        let emptyState = emptyInvestState
                                        stateKey <- GH.insert emptyState
                                        GH.insert_ $ StrategyState stratKey stateKey
                                        return $ Just (strat, Entity (keyToInt stateKey) emptyState)
                                    _ -> return Nothing
                            _ -> return Nothing
                -- Get the active strategy and state for AutoFilters and P2PPicks
                af <- getStratState afOwnerField
                p2pStrat <- getStratState p2pOwnerField
                return (af, p2pStrat)

            (chans, scriptThread, tInvestState, investStateThread) <- do
                -- Activate strategy threads
                let processingQueue AutoFilterStrategy = prosperProcessingQueue ps
                    processingQueue (P2PPicksStrategy ProfitMax) = pmaxResults
                    processingQueue (P2PPicksStrategy LossMin) = lminResults

                    strategyThreads evalStrat stratType (strat, investState) = do
                        -- Duplicate the read listings TChan
                        pq <- atomically . dupTChan $ processingQueue stratType
                        -- Create TVar for InvestState
                        tInvestState <- newTVarIO investState
                        -- Interpret the NoteScript to a Prosper script
                        let script l = prosperScript userinfo l (runStrategy evalStrat tInvestState strat)

                        -- Initialize updating of the InvestState
                        investStateThread <- forkIO . forever $ do
                            investStateNow <- atomically $ do
                                InvestResponse _ _ _ _ amt <- readTChan isr
                                modifyTVar' tInvestState $ fmap (newInvestment amt)
                                readTVar tInvestState
                            flip runDbConn g $
                                replaceEntity investStateNow
                        -- Initialize response handling
                        responseThread <- forkIO . forever $ do
                            investResp <- atomically $ readTChan r
                            runDbConn (insertResponse login stratType investResp) g
                            strategyTypeResponse stratType investResp
                        -- Execute strategy
                        scriptThread <- runProsper ps . forkP . forever $ do
                            ls <- liftIO . atomically $ readTChan pq
                            forkP $ do
                                mResp <- script ls
                                maybe (return ()) (liftIO . atomically . writeTChan i <=< waitProsper) mResp

                        debugM "Account Init" $ "Activating strategy for user: " ++ show login
                        return
                            ( ProsperChans (Just pq) i r
                            , Just (scriptThread, responseThread)
                            , Just tInvestState
                            , Just investStateThread )

                    -- Response handling based on the strategy type.
                    -- Send reports to P2PPicks.
                    strategyTypeResponse (P2PPicksStrategy p2pType)
                        ir@(InvestResponse { investStatus = Success }) = do
                        Just p2pacct <- runDbConn (getP2PPicksAccount login) g
                        let lid = investListingId ir
                            amt = requestedAmount ir
                            sid = subscriberId p2pacct

                        -- TODO Handle failed report to P2P Picks
                        reportResult <- flip runReaderT p2pKeys $ reportInvestment sid p2pType lid amt
                        debugM "P2PPicks" $ "Sending report to P2PPicks " ++ show reportResult
                    strategyTypeResponse _ _ = return ()

                case (mAf, mP2P) of
                    (Just af, _) -> strategyThreads autoFilter AutoFilterStrategy af
                    (_, Just p2pStratState@(p2pStrat, _)) ->
                        -- Assume that p2ppicks account is active...
                        strategyThreads
                            (const $ return True) -- Always invest if it comes from the right TChan
                            (P2PPicksStrategy (strategy p2pStrat))
                            p2pStratState
                    _ -> return (ProsperChans Nothing i r, Nothing, Nothing, Nothing)

            let acct = PTA userinfo chans tInvestState investStateThread scriptThread
            return $ H.insert login acct m
    newAccount m _ = return m

main :: IO ()
main = do
    ps <- initializePeerTrader

    config <- load [ Required "devel.cfg" ]
    p2ppicksConfig <- load [ Optional "snaplets/p2ppicks/devel.cfg" ]

    p2p <- initializeP2PPicks "snaplets/p2ppicks" p2ppicksConfig
    p2pkeys <- initializeP2PPicksKeys config

    dbName <- C.lookupDefault defaultDBName config "groundhog.name"
    dbUser <- C.lookupDefault defaultDBUser config "groundhog.user"
    dbHost <- C.lookupDefault defaultDBHost config "groundhog.host"

    let connString = "dbname=" ++ dbName ++ " user=" ++ dbUser ++ " host=" ++ dbHost
    g <- createPostgresqlPool connString 3

    -- P2P Picks results
    pmaxResults <- newBroadcastTChanIO
    lminResults <- newBroadcastTChanIO

    accts <- initializeDatabase g ps p2pkeys pmaxResults lminResults

    let ops = Ops ps accts g p2p p2pkeys pmaxResults lminResults
    _ <- forkIO $ scheduleLoop ops
    _ <- forkIO $ listingResultLoop ops
    _ <- forkIO $ p2pPicksLoop ops
    startServer ops

    return ()
  where
    defaultDBHost = "localhost"
    defaultDBName = "peertrader"
    defaultDBUser = "peertrader"
