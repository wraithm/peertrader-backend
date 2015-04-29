module PeerTrader.Strategy where

import           Control.Monad                       (forM_)

import           Database.Groundhog
import           Database.Groundhog.Core             as GH
import           Database.Groundhog.Utils.Postgresql (keyToInt)

import           Application
import           PeerTrader.Database
import           PeerTrader.Strategy.Strategy
import           PeerTrader.Types                    (UserLogin)

-- | Get all of the 'Strategy' entities for a user
queryStrategies :: PersistField a => UserLogin -> AppHandler [Entity (Strategy a)]
queryStrategies n = runGH $ selectEntity StrategyConstructor $ OwnerField ==. n

-- | Insert a new strategy into the database, create an empty 'InvestState'
insertStrategy :: PersistField a => UserLogin -> Strategy a -> AppHandler (Maybe Int)
insertStrategy n s
    | n == owner s = fmap (Just . keyToInt) $ runGH $ do
        stratKey <- insert s
        stateKey <- insert emptyInvestState
        insert_ $ StrategyState stratKey stateKey
        return stratKey
    | otherwise = return Nothing

-- | Update a strategy
updateStrategy :: PersistField a => Entity (Strategy a) -> AppHandler ()
updateStrategy = runGH . replaceEntity

-- | Delete a Strategy by the key
deleteStrategy :: PersistField a => AutoKey (Strategy a) -> AppHandler ()
deleteStrategy stratKey = runGH $ do
    stratStates <- project (AutoKeyField, StrategyStateConstructor) $ 
        StrategyRefField ==. stratKey
    forM_ stratStates $ \(refKey, StrategyState stratKey' stateKey) -> do
        deleteBy refKey
        deleteBy stratKey'
        deleteBy stateKey
    -- FIXME Redundant?
    deleteBy stratKey
    delete $ StrategyRefField ==. stratKey
