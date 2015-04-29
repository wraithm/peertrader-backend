{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Socket.Activity where

import           Network.WebSockets
import           Network.WebSockets.Snap

import           Control.Concurrent.STM         (TChan, atomically, readTChan)
import           Control.Monad                  (forever)
import           Control.Monad.IO.Class         (liftIO)

import           Data.Aeson
import qualified Data.Foldable                  as F
import           Data.Text                      (unpack)
import           Data.Time.Clock                (UTCTime, getCurrentTime)

import           Logging                        (infoM)
import           Prosper.Invest

import           Application
import           PeerTrader.Account.Web
import           PeerTrader.Investment.Database
import           PeerTrader.Types               (UserLogin)

investJSON :: UTCTime -> InvestResponse -> Value
investJSON t ir = object
    [ "amount" .= amountInvested ir
    , "listingId" .= investListingId ir
    , "status" .= investStatus ir
    , "message" .= investMessage ir
    , "insufficientFunds" .=
        (investMessage ir == InsufficientFunds)
    , "timeStamp" .= t
    ] -- Need status

investmentJSON :: Investment -> Value
investmentJSON (Investment _ t _ ir) = investJSON t ir

sendJSON :: ToJSON a => Connection -> a -> IO ()
sendJSON conn = sendTextData conn . encode

activity :: AppHandler ()
activity = withCurrentUser_ $ \n -> do
    invts <- getAccountVar n prosperResponses
    latestResponses <- runGH (userResponses n)
    runWebSocketsSnap (activitySocket n latestResponses invts)

activitySocket :: UserLogin -> [Investment] -> Maybe (TChan InvestResponse) -> ServerApp
activitySocket n responses invts pending = do
    logWS $ "Initializing connection for " ++ user
    conn <- acceptRequestWith pending (AcceptRequest (Just "Sec-WebSocket-Protocol"))
    msg <- receiveDataMessage conn -- Initialize connection
    logWS $ show msg ++ " Connection established for " ++ user

    mapM_ (sendJSON conn . investmentJSON) responses

    -- TODO Need a way to start sending JSON after a strategy has been activated.
    -- Perhaps investments should not be a Maybe.
    -- I think this ^^^ error is fixed now... Need to test.
    F.forM_ invts $ \i -> forever $ do
        investResponse <- liftIO . atomically $ readTChan i
        logWS $ "Sending invest response data to " ++ user ++ ": " ++ show investResponse
        now <- getCurrentTime
        (sendJSON conn . investJSON now) investResponse
  where
    logWS = infoM "WebSocket"
    user = unpack n
