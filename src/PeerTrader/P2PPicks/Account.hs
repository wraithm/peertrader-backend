{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module PeerTrader.P2PPicks.Account where

import           Data.ByteString       (ByteString)
import           Data.Maybe            (listToMaybe)

import           Database.Groundhog    (PersistBackend, select, (==.))
import           Database.Groundhog.TH

import           P2PPicks

import           PeerTrader.Types      (UserLogin)

data P2PPicksAccount = P2PPicksAccount
    { p2ppicksOwner    :: UserLogin
    , p2ppicksUser     :: ByteString
    , p2ppicksPass     :: ByteString
    , subscriberId     :: SubscriberID
    , p2ppicksIsActive :: Bool
    }

mkPersist defaultCodegenConfig [groundhog|
entity: P2PPicksAccount
constructors:
  - name: P2PPicksAccount
    fields:
      - name: p2ppicksOwner
        type: text
        reference:
          table: snap_auth_user
          columns: [login]
    uniques:
      - name: oneuserperp2ppicks
        fields: [p2ppicksOwner]
|]

getP2PPicksAccount :: PersistBackend m => UserLogin -> m (Maybe P2PPicksAccount)
getP2PPicksAccount n = do
    accts <- select $ P2ppicksOwnerField ==. n
    return (listToMaybe accts)
