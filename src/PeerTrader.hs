{-# LANGUAGE FlexibleContexts #-}

module PeerTrader where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM         (atomically, dupTChan,
                                                 readTChan, writeTChan)
import           Control.Exception              (SomeException)
import           Control.Exception.Enclosed     (catchAny)
import           Control.Monad.Reader

import           Data.Maybe                     (isNothing)
import           Data.Traversable               as T

import           Database.Groundhog.Postgresql  (runDbConn)

import           Logging
import           NoteScript
import           P2PPicks.Keys
import           P2PPicks.Types
import           Prosper
import           Prosper.Monad

import           PeerTrader.Account
import           PeerTrader.Investment.Database
import           PeerTrader.Ops
import           PeerTrader.P2PPicks.Account
import           PeerTrader.StrategyType
import           PeerTrader.Types               (UserLogin)

-- | Given 'Prosper' actions per 'Listing', read from the 'Listing' 'TChan'
-- and automatically invest based upon those Listings.
--
-- Write all 'InvestResponse' to the database
processListings
    :: UserLogin
    -> StrategyType
    -> (Listing -> Prosper (Maybe (Async InvestResponse)))
    -> OpsReader (Maybe (ThreadId, ThreadId))
processListings n stratType s =
    getAccountVar n prosperChans >>= setUpChans
  where
    processingQueue AutoFilterStrategy = do
        ps <- asks _prosperState
        return $ prosperProcessingQueue ps
    processingQueue (P2PPicksStrategy ProfitMax) = asks _p2pProfitMaxResults
    processingQueue (P2PPicksStrategy LossMin) = asks _p2pLossMinResults

    setUpChans (Just (ProsperChans (Just pq) i r)) = initializeThreads pq i r
    setUpChans (Just (ProsperChans Nothing i r)) = do
        -- Duplicate the read listings TChan
        pq <- processingQueue stratType
        pq' <- (liftIO . atomically . dupTChan) pq

        putAccountVar n prosperChans $ ProsperChans (Just pq') i r
        initializeThreads pq' i r
    setUpChans Nothing =
        warningM (show n) "Could not retrieve Prosper Chans!" >>
        return Nothing

    initializeThreads pq is readInvests = do
        responseThread <- do
            app <- ask
            liftIO . forkIO . forever $ do
                investResp <- atomically $ readTChan readInvests
                runDbConn (insertResponse n stratType investResp) app
                strategyTypeResponse app stratType investResp
        scriptThread <- evalProsper . forkP . forever $ do
            ls <- liftIO . atomically $ readTChan pq
            forkP $ do
                mResp <- s ls
                maybe (return ()) (liftIO . atomically . writeTChan is <=< waitProsper) mResp
        return $ Just (scriptThread, responseThread)

    strategyTypeResponse app (P2PPicksStrategy p2pType)
        ir@(InvestResponse { investStatus = Success }) =
        flip runReaderT app $ do
        keys <- asks _p2ppicksKeys
        Just p2pacct <- runDbConn (getP2PPicksAccount n) app
        let lid = investListingId ir
            amt = requestedAmount ir
            sid = subscriberId p2pacct

        -- TODO Handle failed report to P2P Picks
        reportResult <- flip runReaderT keys $ reportInvestment sid p2pType lid amt
        debugM "P2PPicks" $ "Sending report to P2PPicks " ++ show reportResult
    strategyTypeResponse _ _ _ = return ()

-- | Interpret a NoteScript into Prosper commands
startProsperScript
    :: UserLogin
    -> StrategyType
    -> ProsperScript (Maybe (Async InvestResponse))
    -> OpsReader (Maybe (ThreadId, ThreadId))
startProsperScript n stratType pscript = do
    tid <- join <$> getAccountVar n prosperScriptThread
    ui <- getAccountVar n prosperUser

    -- Kill current NoteScript
    _ <- T.mapM (liftIO . (\(t1, t2) -> killThread t1 >> killThread t2)) tid

    case ui of
        Just i -> do
            let lg = show n
            debugM lg $ "Setting prosper script for user: " ++ show n
            newTid <- processListings n stratType (\l -> prosperScript i l pscript)

            when (isNothing newTid) $ warningM lg "Unable to set new thread..."

            putAccountVar n prosperScriptThread newTid
            return newTid
        _ -> warningM (show n) "Could not find prosper user." >> return Nothing

-- | Kill the strategy thread. Stop automatically investing.
killProsper :: UserLogin -> OpsReader ()
killProsper n = do
    tid <- join <$> getAccountVar n prosperScriptThread
    stateTid <- join <$> getAccountVar n prosperInvestThread
    _ <- T.mapM (liftIO . (\(t1, t2) -> killThread t1 >> killThread t2)) tid
    _ <- T.mapM (liftIO . killThread) stateTid
    putAccountVar n prosperScriptThread Nothing
    putAccountVar n (prosperChans.readListings) Nothing
    putAccountVar n prosperInvestThread Nothing
    putAccountVar n prosperInvestState Nothing

initializeProsper' :: IO ProsperState
initializeProsper' = do
    ps <- initializeProsper "prosper.cfg"

    _ <- runProsper ps updateAllListings -- Synchronize
    _ <- forkIO $ updateListingsLoop ps

    return ps
  where
    -- TODO Do something with the retry package here. Need to retry at a non-constant rate.
    updateListingsLoop :: ProsperState -> IO ()
    updateListingsLoop ps = forever $
        runProsper ps updateAllListings `catchAny` statusExceptionHandler

    statusExceptionHandler :: SomeException -> IO ()
    statusExceptionHandler e = debugM "MarketData" $
        "Caught some exception... " ++ show e ++ " Continuing..."

-- | Initialize logs and initialize ProsperState
--
-- ProsperState reads configuration from prosper.cfg. This involves initializing
-- Listings poll thread
initializePeerTrader :: MonadIO m => m ProsperState
initializePeerTrader = liftIO $ do
    initializeLogs

    debugM "Prosper" "Initializing Prosper..."
    initializeProsper'
