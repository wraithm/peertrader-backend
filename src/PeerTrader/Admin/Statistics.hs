{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PeerTrader.Admin.Statistics where

import           Control.Monad.Logger                         (runNoLoggingT)
import           Control.Monad.Reader

import qualified Data.Foldable                                as F
import           Data.Time.Clock                              (getCurrentTime)
import qualified Data.Traversable                             as T

import           Database.Groundhog
import           Database.Groundhog.Core
import           Database.Groundhog.Generic                   (mapAllRows)

import           NoteScript                                   (Money)
import           P2PPicks.Types                               (P2PPicksType)
import           Prosper.Invest

import           Application
import           PeerTrader.Admin.Database
import           PeerTrader.AutoFilter.AutoFilter             (AutoFilter)
import           PeerTrader.Database                          (runDb)
import           PeerTrader.Investment.Database
import           PeerTrader.Prosper.Account                   (ProsperAccount,
                                                               saveAccounts)
import           PeerTrader.Strategy.Strategy                 (Strategy)
import           PeerTrader.StrategyManagement.ActiveStrategy (ActiveStrategy)

handleUpdateStats :: AppHandler ()
handleUpdateStats = ask >>=
    liftIO . runNoLoggingT . runReaderT
        (saveAccounts >> updateStats)

updateStats :: AppReader ()
updateStats = do
    now <- liftIO getCurrentTime
    numActive <- numStrategiesActive
    numStrats <- numStrategies
    sumInvested <- sumAmountInvested
    sumValues <- sumAccountValues
    runDb $ insert_ $ Statistics now numActive numStrats sumValues sumInvested

sumJusts :: (T.Traversable t, Num a, Functor f, Monad f) => t (f a) -> f a
sumJusts = fmap F.sum . T.sequence

numStrategiesActive :: AppReader Int
numStrategiesActive = runDb $ do
    p2pPicks <- countAll (undefined :: ActiveStrategy P2PPicksType)
    af <- countAll (undefined :: ActiveStrategy AutoFilter)
    return (af + p2pPicks)

numStrategies :: AppReader Int
numStrategies = runDb $ countAll (undefined :: Strategy AutoFilter)

-- | TODO Ugly hack...
sumAccountValues :: AppReader Money
sumAccountValues = do
    numAccounts <- runDb $ countAll (undefined :: ProsperAccount)
    if numAccounts > 0
        then do
            sumValues <- runDb $
                queryRaw False sumAcctValueQuery [] (mapAllRows (liftM fst . fromPersistValues))
            case sumValues of
                [] -> return 0
                x:_ -> return x
        else return 0
  where
    sumAcctValueQuery =
        "select sum(\"account#totalAccountValue\") from \"ProsperAccount\" where (owner, \"timeStamp\") in (select owner, max(\"timeStamp\") as \"timeStamp\" from \"ProsperAccount\" group by owner)"
{- This doesn't work...
sumAccountValues = do
    values <- runDb $
        project (AccountField ~> TotalAccountValueSelector) $
            (P.OwnerField, P.TimeStampField) `in_` [P.OwnerField, mkExpr $ function "max" [toExpr P.TimeStampField]]
    return $ sum values
-}

sumAmountInvested :: AppReader Money
sumAmountInvested = do
    invts <- runDb selectAll
    let amountsInvested = map (amountInvested . investment . snd) invts
    return $ sum amountsInvested
