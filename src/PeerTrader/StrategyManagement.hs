module PeerTrader.StrategyManagement where

import           Control.Applicative                          ((<$>))
import           Control.Monad                                (liftM)

import           Data.Maybe                                   (listToMaybe)

import           Database.Groundhog
import           Database.Groundhog.Core

import           P2PPicks.Types                               (P2PPicksType)

import           PeerTrader.AutoFilter.AutoFilter
import           PeerTrader.Strategy.Strategy
import           PeerTrader.StrategyManagement.ActiveStrategy
import           PeerTrader.Types                             (UserLogin)

anActiveStrategy 
    :: (PersistField a, PersistBackend m) 
    => UserLogin 
    -> m (Maybe (Strategy a))
anActiveStrategy n = do
    activeStrat <- liftM listToMaybe $ select $ ActiveOwnerField ==. n
    let afKey = activeStrategy <$> activeStrat
    getMaybeKey afKey
  where
    getMaybeKey (Just k) = get k
    getMaybeKey Nothing = return Nothing

currentStrategy 
    :: PersistBackend m 
    => UserLogin
    -> m (Maybe (Strategy (Either AutoFilter P2PPicksType)))
currentStrategy n = do
    af <- anActiveStrategy n
    p2p <- anActiveStrategy n
    case (af, p2p) of
        (Just a, _) -> return $ Just $ fmap Left a
        (_, Just a) -> return $ Just $ fmap Right a
        _ -> return Nothing

-- TODO This is a total hack. We should rethink how to flow invest
-- and strategy data to the client.
currentInvestState :: PersistBackend m => UserLogin -> m (Maybe InvestState)
currentInvestState n = do
    afStrat <- liftM listToMaybe $ select $ afOwnerField ==. n
    p2pStrat <- liftM listToMaybe $ select $ p2pOwnerField ==. n
    case (afStrat, p2pStrat) of
        (Just activeStrat, _) -> selectInvestState activeStrat
        (_, Just activeStrat) -> selectInvestState activeStrat
        _ -> return Nothing
  where
    selectInvestState :: (PersistBackend m, PersistField a) => ActiveStrategy a -> m (Maybe InvestState)
    selectInvestState (ActiveStrategy _ stratKey) = do
        stateKeys <- project InvestStateRefField $ StrategyRefField ==. stratKey
        case stateKeys of
            stateKey:_ -> get stateKey
            _ -> return Nothing
