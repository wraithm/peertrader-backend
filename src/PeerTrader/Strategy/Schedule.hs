module PeerTrader.Strategy.Schedule (updateDailyInvested) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader         (asks)

import           Data.Traversable             as T

import           Database.Groundhog

import           NoteScript                   (Money)

import           PeerTrader.Account
import           PeerTrader.Database          (runDb)
import           PeerTrader.Ops
import           PeerTrader.Strategy.Strategy

updateDailyInvested :: OpsReader ()
updateDailyInvested = do
    accts <- liftIO . readTVarIO =<< asks _accounts
    _ <- T.traverse resetInvestState accts
    resetDatabase

resetDatabase :: OpsReader ()
resetDatabase = runDb $ update [DailyInvestedField  =. (0 :: Money)] CondEmpty

resetInvestState :: PeerTraderAccount -> OpsReader ()
resetInvestState (PTA { _prosperInvestState = Just tInvestState }) =
    liftIO . atomically $
        modifyTVar' tInvestState $ fmap resetDaily
resetInvestState (PTA { _prosperInvestState = Nothing }) = return ()
