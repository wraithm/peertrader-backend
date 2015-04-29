{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Route.P2PPicks (p2ppicksHandler) where

import           Snap.Core
import           Snap.Extras.JSON                    (writeJSON)
import           Snap.Extras.TextUtils               (readBS)

import           Control.Applicative                 ((<|>))

import           Data.Aeson
import qualified Data.Text                           as T

import           Database.Groundhog                  (AutoKey)
import           Database.Groundhog.Utils.Postgresql (intToKey)

import           Application
import           Logging                             (debugM)
import           P2PPicks.Types                      (P2PPicksType)

import           PeerTrader.Database
import           PeerTrader.Socket.Web               (deactivateStrategy)
import           PeerTrader.Strategy
import           PeerTrader.Strategy.JSON
import           PeerTrader.Strategy.Strategy

-- | 'GET' requests go to 'autoFilterStatus'
--   'POST' requests go to 'acceptAF'
--   'PUT' requests go to 'putAF'
--   'DELETE' requests go to 'deleteAutoFilter'
p2ppicksHandler :: AppHandler ()
p2ppicksHandler
     =  method GET p2ppicksStatus
    <|> method POST acceptP2PPicks
    <|> method PUT putP2PPicks
    <|> method DELETE deleteP2PPicks

p2ppicksStatus :: AppHandler ()
p2ppicksStatus = withCurrentUser_ $ \n -> do
    p2ps <- queryStrategies n :: AppHandler [Entity (Strategy P2PPicksType)]
    writeJSON p2ps

acceptP2PPicks :: AppHandler ()
acceptP2PPicks = withCurrentUser_ $ \n -> do
    p2p <- addOwnerValidate n :: AppHandler (Strategy P2PPicksType)
    debugM "P2PPicks" $
        "Received p2ppicks from user (" ++ T.unpack n ++ "): " ++ show p2p

    Just p2pid <- insertStrategy n p2p
    writeJSON $ object [ "id" .= p2pid ]

putP2PPicks :: AppHandler ()
putP2PPicks = withCurrentUser_ $ \n -> do
    p2p <- addOwnerValidate n :: AppHandler (Entity (Strategy P2PPicksType))
    debugM "P2PPicks" $
        "Received update to p2ppicks from user (" ++ T.unpack n ++ "): " ++ show p2p

    -- kill thread, update in database
    deactivateStrategy n
    updateStrategy p2p

deleteP2PPicks :: AppHandler ()
deleteP2PPicks = withCurrentUser_ $ \n -> do
    Just p2pid <- getParam "id"
    debugM "P2PPicks" $
        "Delete p2ppicks from user (" ++ T.unpack n ++ "): " ++ show p2pid
    deactivateStrategy n
    let stratKey = intToKey (readBS p2pid) :: AutoKey (Strategy P2PPicksType)
    deleteStrategy stratKey
