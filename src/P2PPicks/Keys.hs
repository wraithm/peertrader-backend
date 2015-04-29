{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module P2PPicks.Keys
    ( initializeP2PPicksKeys 
    , validateUser
    , subscriberStatus
    , reportInvestment
    , SubscriberID
    , P2PPicksKeys (..)
    ) where

import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Reader    (MonadReader, ask)

import           Data.ByteString         (ByteString)
import           Data.Configurator       (lookupDefault)
import           Data.Configurator.Types (Config)

import           P2PPicks.Request        (APIKey, APISecret, SubscriberID)
import qualified P2PPicks.Request        as P
import           P2PPicks.Types          (P2PPicksType)

data P2PPicksKeys = P2PPicksKeys
    { apiKey    :: APIKey
    , apiSecret :: APISecret
    } deriving Show

validateUser
    :: (MonadIO m, MonadReader P2PPicksKeys m)
    => ByteString
    -> ByteString
    -> m (Maybe (SubscriberID, Bool))
validateUser email password = do
    P2PPicksKeys {..} <- ask
    P.validateUser apiKey apiSecret email password

subscriberStatus
    :: (MonadIO m, MonadReader P2PPicksKeys m)
    => SubscriberID
    -> m Bool
subscriberStatus subscriberId = do
    P2PPicksKeys {..} <- ask
    P.subscriberStatus apiKey apiSecret subscriberId

reportInvestment
    :: (MonadIO m, MonadReader P2PPicksKeys m)
    => SubscriberID
    -> P2PPicksType -- ^ Strategy type
    -> Int -- ^ Listing ID
    -> Double -- ^ Amount
    -> m Bool
reportInvestment subscriberId picksType listingId amt = do
    P2PPicksKeys {..} <- ask
    P.reportInvestment apiKey apiSecret subscriberId picksType listingId amt

initializeP2PPicksKeys :: Config -> IO P2PPicksKeys
initializeP2PPicksKeys config = do
    apiKey <- lookupDefault "" config "p2ppicks.apikey"
    apiSecret <- lookupDefault "" config "p2ppicks.apisecret"

    return P2PPicksKeys {..}
