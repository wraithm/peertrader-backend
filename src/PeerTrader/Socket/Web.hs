{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Socket.Web where

import           System.ZMQ4.Monadic                          as Z

import           Snap.Extras.CoreUtils                        (finishEarly)
import           Snap.Snaplet                                 (with)
import           Snap.Snaplet.P2PPicks

import           Control.Concurrent                           (threadDelay)
import           Control.Concurrent.Async                     as A
import           Control.Concurrent.STM
import           Control.Exception.Enclosed
import           Control.Monad                                (forever, join,
                                                               void)
import           Control.Monad.Reader                         (MonadIO, asks)
import           Control.Monad.Util                           (ifM)

import           Data.HashMap.Strict                          as H
import           Data.Serialize                               (decode, encode)
import           Data.Traversable                             as T

import           Application
import           Logging                                      (debugM, infoM,
                                                               warningM)
import           PeerTrader.Account.Web
import           PeerTrader.P2PPicks.Account
import           PeerTrader.Socket.Command
import           PeerTrader.StrategyManagement.SelectStrategy
import           PeerTrader.Types
import           Prosper                                      (User (..))

startClient :: TChan Command -> Accounts -> IO ()
startClient commandChan accts = runZMQ $ do
    commandSock <- socket Req
    connect commandSock "tcp://localhost:1234"

    responseSock <- socket Sub
    connect responseSock "tcp://localhost:1235"
    subscribe responseSock ""
    infoM "OpsConnection" "Connection established."

    (decodeAsync, processAsync) <- readResponses accts responseSock
    commandsAsync <- Z.async $ processCommands commandChan commandSock

    void . Z.async . liftIO $ do
        _ <- A.waitAnyCatchCancel [decodeAsync, processAsync, commandsAsync]
        infoM "OpsConnection" "Connection lost."
        reconnect
  where
    reconnect = startClient commandChan accts `catchAny` reconnectError
    reconnectError _ = do
        warningM "OpsConnection" "Could not reconnect."
        threadDelay 500000
        reconnect

enqueue :: MonadIO m => TChan a -> a -> m ()
enqueue chan = liftIO . atomically . writeTChan chan

readResponses :: Receiver t => Accounts -> Socket z t -> ZMQ z (Async (), Async ())
readResponses accts responseSock = do
    responseChan <- liftIO $ atomically newTChan
    decodeAsync <- Z.async . forever $ do
        encodedCommand <- receive responseSock
        case decode encodedCommand of
            Left s -> error s
            Right x -> enqueue responseChan x
    processAsync <- liftIO . A.async $ processResponse accts responseChan
    return (decodeAsync, processAsync)

processResponse :: Accounts -> TChan Response -> IO ()
processResponse taccts responseChan = forever $ do
    (Response n ir, accts) <- atomically $ do
        response <- readTChan responseChan
        accts <- readTVar taccts
        return (response, accts)
    let mResponses = _prosperResponses <$> H.lookup n accts
    T.mapM (`enqueue` ir) mResponses

processCommands :: (Receiver t, Sender t) => TChan Command -> Socket z t -> ZMQ z ()
processCommands commandChan commandSock = forever $ do
    command <- liftIO . atomically $ readTChan commandChan
    send commandSock [] (encode command)
    debugM "OpsConnection" $ "Sending " ++ show command
    void $ receive commandSock

sendCommand :: Command -> AppHandler ()
sendCommand command = do
    commandChan <- asks _opsWriteChan
    liftIO . atomically $ writeTChan commandChan command

deactivateStrategy :: UserLogin -> AppHandler ()
deactivateStrategy = sendCommand . Deactivate

activateStrategy :: UserLogin -> SelectStrategy -> AppHandler ()
activateStrategy n (AutoFilterStrategy sid _ _) =
    sendCommand (ActivateAutoFilter n sid)
activateStrategy n (P2PPicksStrategy sid _ _) =
    join <$> getAccountVar n p2ppicksAccount >>=
    maybe noP2PPicksAcct checkUser
  where
    checkUser :: P2PPicksAccount -> AppHandler ()
    checkUser (P2PPicksAccount { subscriberId = subid }) =
        ifM (with p2ppicks (subscriberStatus subid))
            (sendCommand (ActivateP2PPicks n sid))
            (finishEarly 401 "Subscriber not valid.")

    noP2PPicksAcct = finishEarly 401 "Could not find P2P Picks Account."

newProsperUser :: UserLogin -> User -> AppHandler ()
newProsperUser n (User un pw) = sendCommand (NewProsperAccount n un pw)

deleteProsperUser :: UserLogin -> AppHandler ()
deleteProsperUser = sendCommand . DeleteProsperAccount
