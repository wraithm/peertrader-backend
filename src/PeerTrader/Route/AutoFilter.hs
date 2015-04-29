{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Route.AutoFilter (autoFilterHandler) where

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

import           PeerTrader.AutoFilter.AutoFilter
import           PeerTrader.Database
import           PeerTrader.Socket.Web               (deactivateStrategy)
import           PeerTrader.Strategy
import           PeerTrader.Strategy.JSON
import           PeerTrader.Strategy.Strategy

-- | 'GET' requests go to 'autoFilterStatus'
--   'POST' requests go to 'acceptAF'
--   'PUT' requests go to 'putAF'
--   'DELETE' requests go to 'deleteAutoFilter'
autoFilterHandler :: AppHandler ()
autoFilterHandler
     =  method GET autoFilterStatus
    <|> method POST acceptAF
    <|> method PUT putAF
    <|> method DELETE deleteAutoFilter

-- | Get all of the autofilters for the current user.
--
-- There are no parameters to this handler.
--
-- The body of the response contains a JSON serialized 'AutoFilter'.
--
-- 500 error if there's a problem.
--
-- Note: I generally try to avoid monad operators other than >>=.
-- I think that this is an alright use. queryAutoFilters >=> writeJSON
-- can be replaced by \n -> queryAutoFilters n >>= writeJSON
autoFilterStatus :: AppHandler ()
autoFilterStatus = withCurrentUser_ $ \n -> do
    afs <- queryStrategies n :: AppHandler [Entity (Strategy AutoFilter)]
    writeJSON afs

-- | Insert new autofilters into the database.
--
-- The body of a POST request to this handler must contain a valid 'AutoFilter'
-- data structure (in JSON). The 'afId' field will be ignored.
--
-- The body of the response contains a JSON object { afId: int }, where int
-- is the afId of the 'AutoFilter' from the database.
--
-- This handler will 500 error if there's a problem.
acceptAF :: AppHandler ()
acceptAF = withCurrentUser_ $ \n -> do
    af <- addOwnerValidate n :: AppHandler (Strategy AutoFilter)
    debugM "AutoFilter" $
        "Received autofilter from user (" ++ T.unpack n ++ "): " ++ show af

    -- insert into db
    Just afid <- insertStrategy n af
    writeJSON $ object [ "id" .= afid ]

-- | Update an existing autofilter in the database.
--
-- The body of the PUT request to this handler must contain a valid 'AutoFilter'
-- with an existing afId in the database.
--
-- There's no response body.
--
-- 500 error if something goes wrong.
putAF :: AppHandler ()
putAF = withCurrentUser_ $ \n -> do
    af <- addOwnerValidate n :: AppHandler (Entity (Strategy AutoFilter))
    debugM "AutoFilter" $
        "Received update to autofilter from user (" ++ T.unpack n ++ "): " ++ show af

    -- update in database, kill thread
    deactivateStrategy n
    updateStrategy af

-- | Delete an existing autofilter via afId in the database.
--
-- This handler only takes one parameter, afId. It must be an Int.
--
-- There's no response body.
--
-- 500 error if something goes wrong.
deleteAutoFilter :: AppHandler ()
deleteAutoFilter = withCurrentUser_ $ \n -> do
    Just afid <- getParam "id"
    debugM "AutoFilter" $
        "Delete autofilter from user (" ++ T.unpack n ++ "): " ++ show afid
    deactivateStrategy n
    let stratKey = intToKey (readBS afid) :: AutoKey (Strategy AutoFilter)
    deleteStrategy stratKey
