{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Socket.Ops (startServer) where

import           System.ZMQ4.Monadic

import           Control.Concurrent                           (forkIO,
                                                               killThread)
import           Control.Concurrent.STM
import           Control.Monad                                (forever, join,
                                                               void)
import           Control.Monad.Logger                         (runNoLoggingT)
import           Control.Monad.Reader                         (MonadIO, ask,
                                                               runReaderT)

import qualified Data.HashMap.Strict                          as H
import           Data.Serialize                               (decode, encode)
import           Data.Traversable                             as T

import           Database.Groundhog                           as GH
import           Database.Groundhog.Core                      (PersistField)
import           Database.Groundhog.Generic                   (runDbConn)
import           Database.Groundhog.Utils.Postgresql          (intToKey,
                                                               keyToInt)

import           Logging                                      (debugM)
import           NoteScript                                   (ProsperScript)
import           P2PPicks.Types                               (P2PPicksType)
import           Prosper.Invest
import           Prosper.User

import           PeerTrader                                   (killProsper, startProsperScript)
import           PeerTrader.Account
import           PeerTrader.AutoFilter.AutoFilter             (AutoFilter,
                                                               autoFilter)
import           PeerTrader.Database                          (Entity (Entity),
                                                               replaceEntity,
                                                               runDb)
import           PeerTrader.Ops
import           PeerTrader.Socket.Command
import           PeerTrader.Strategy.Strategy
import           PeerTrader.StrategyManagement.ActiveStrategy
import           PeerTrader.StrategyType
import           PeerTrader.Types                             (UserLogin)


startServer :: Ops -> IO ()
startServer ops = do
    commandChan <- atomically newTChan
    void . forkIO $ processCommand ops commandChan
    runZMQ $ do
        commandSock <- socket Rep
        bind commandSock "tcp://*:1234"

        responseSock <- socket Pub
        bind responseSock "tcp://*:1235"

        handleConn ops commandChan commandSock responseSock

handleConn :: (Sender t1, Sender t2, Receiver t1) => Ops -> TChan Command -> Socket z t1 -> Socket z t2 -> ZMQ z ()
handleConn ops commandChan commandSock responseSock = do
    accts <- liftIO $ readTVarIO (_accounts ops)
    _ <- H.traverseWithKey (sendResponses responseSock) accts

    forever $ do
        command <- receive commandSock
        case decode command of
            Right c -> do
                enqueue commandChan c
                send commandSock [] "OK"
            Left err -> liftIO $ debugM "WebConnection" $
                "Could not decode command: " ++ err
  where
    sendResponses :: Sender t => Socket z t -> UserLogin -> PeerTraderAccount -> ZMQ z ()
    sendResponses sock n acct = do
        investmentRead <- liftIO . atomically $ dupTChan (_investWrite . _prosperChans $ acct)
        void . async . forever $ do
            investResponse <- liftIO . atomically $ readTChan investmentRead
            debugM "WebConnection" "Sending invest response..."
            send sock [] (encode (Response n investResponse))

enqueue :: MonadIO m => TChan a -> a -> m ()
enqueue chan = liftIO . atomically . writeTChan chan

processCommand :: Ops -> TChan Command -> IO ()
processCommand ops commandChan = forever $ do
    command <- atomically (readTChan commandChan)
    debugM "WebConnection" $ "Processed " ++ show command
    void . forkIO $
        runOpsDbReader (executeCommand command)
  where
    runOpsDbReader = runNoLoggingT . flip runReaderT ops

activateCommand
    :: PersistField a
    => (a -> ProsperScript Bool)
    -> UserLogin
    -> StrategyType
    -> DefaultKey (Strategy a)
    -> OpsReader ()
activateCommand script n stratType stratKey = do
    -- Find the Strategy and InvestState for the strategy that we want to activate
    -- If the state doesn't exist, create one, and insert it into the database
    (strat, state) <- runDb $ do
        Just strat <- get stratKey
        mStates <- getInvestState stratKey
        case mStates of
            Just state -> return (strat, state)
            _ -> do
                let emptyState = emptyInvestState
                stateKey <- GH.insert emptyState
                GH.insert_ $ StrategyState stratKey stateKey
                return (strat, Entity (keyToInt stateKey) emptyState)

    -- Create a TVar for the InvestState
    tState <- liftIO $ newTVarIO state
    putAccountVar n prosperInvestState $ Just tState

    -- Get a new investment response TChan
    Just writeInvests <- getAccountVar n (prosperChans.investWrite)
    readInvests <- liftIO . atomically $ dupTChan writeInvests

    -- Need the App for inserting into the db on the InvestState thread
    investOps <- ask

    -- Kill the previous InvestState thread
    stateTid <- join <$> getAccountVar n prosperInvestThread
    _ <- T.mapM (liftIO . killThread) stateTid

    -- Start the InvestState thread
    -- Update the TVar and Database for InvestState every time there's an investment
    investStateThread <- liftIO . forkIO . forever $ do
        investState <- atomically $ do
            InvestResponse _ _ _ _ amt <- readTChan readInvests
            modifyTVar' tState $ fmap (newInvestment amt)
            readTVar tState
        flip runDbConn investOps $
            replaceEntity investState

    -- Put the InvestState thread into the App
    putAccountVar n prosperInvestThread $ Just investStateThread

    -- Finally, interpret the AutoFilter and run the Strategy
    void $ startProsperScript n stratType (runStrategy script tState strat)
    runDb $ activateStrategy n stratKey

executeCommand :: Command -> OpsReader ()
executeCommand (ActivateAutoFilter n k) =
    activateCommand autoFilter n AutoFilterStrategy stratKey
  where stratKey = intToKey k :: DefaultKey (Strategy AutoFilter)
executeCommand (ActivateP2PPicks n k) = do
    -- TODO Hack to get the P2PPicksType
    Just p2pStrat <- runDb $ get stratKey
    let stratType = P2PPicksStrategy (strategy p2pStrat)
    activateCommand (const $ return True) n stratType stratKey
  where stratKey = intToKey k :: DefaultKey (Strategy P2PPicksType)
executeCommand (Deactivate n) = killStrategy n
executeCommand (NewProsperAccount n un pw) = do
    (i, r) <- liftIO . atomically $ do
        i <- newBroadcastTChan
        r <- dupTChan i
        return (i, r)
    let acct = PTA user (ProsperChans Nothing i r) Nothing Nothing Nothing
    putAccount n acct
  where
    user = User un pw
executeCommand (DeleteProsperAccount n) = do
    killStrategy n
    removeAccount n

killStrategy :: UserLogin -> OpsReader ()
killStrategy n = do
    runDb $ deleteActive n
    killProsper n
