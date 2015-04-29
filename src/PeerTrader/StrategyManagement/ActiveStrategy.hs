{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module PeerTrader.StrategyManagement.ActiveStrategy where

import           Database.Groundhog
import           Database.Groundhog.Core
import           Database.Groundhog.TH

import           P2PPicks.Types                   (P2PPicksType)

import           PeerTrader.AutoFilter.AutoFilter (AutoFilter)
import           PeerTrader.Strategy.Strategy
import           PeerTrader.Types                 (UserLogin)

-- | Table of active strategies.
data ActiveStrategy a = ActiveStrategy
    { activeOwner    :: UserLogin
    , activeStrategy :: DefaultKey (Strategy a)
    }

mkPersist defaultCodegenConfig [groundhog|
entity: ActiveStrategy
|]

afOwnerField :: Field (ActiveStrategy AutoFilter) ActiveStrategyConstructor UserLogin
afOwnerField = ActiveOwnerField

p2pOwnerField :: Field (ActiveStrategy P2PPicksType) ActiveStrategyConstructor UserLogin
p2pOwnerField = ActiveOwnerField

deleteActive
    :: PersistBackend m
    => UserLogin -> m ()
deleteActive n = do
    delete $ afOwnerField ==. n
    delete $ p2pOwnerField ==. n

activateStrategy
    :: (PersistField a, PersistBackend m)
    => UserLogin
    -> DefaultKey (Strategy a)
    -> m ()
activateStrategy n k = do
    deleteActive n
    insert_ $ ActiveStrategy n k
