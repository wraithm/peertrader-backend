module PeerTrader.Route.StrategyManagement where

import           Snap.Core
import           Snap.Extras.JSON              (reqJSON, writeJSON)

import           Control.Applicative           ((<|>))

import           Application
import qualified PeerTrader.Socket.Web         as C
import           PeerTrader.StrategyManagement.SelectStrategy

strategyManagement :: AppHandler ()
strategyManagement =
    method GET availableStrategies <|>
    method POST activateStrategy <|>
    method DELETE killStrategy

availableStrategies :: AppHandler ()
availableStrategies = withCurrentUser $ \n -> do
    strats <- runGH (userStrategies n)
    writeJSON (strats :: [SelectStrategy])

activateStrategy :: AppHandler ()
activateStrategy = withCurrentUser $ \n ->
    reqJSON >>= C.activateStrategy n

killStrategy :: AppHandler ()
killStrategy = withCurrentUser C.deactivateStrategy
