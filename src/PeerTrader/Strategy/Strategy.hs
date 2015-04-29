{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module PeerTrader.Strategy.Strategy where

import           Control.Concurrent.Async
import           Control.Concurrent.STM

import           Data.Aeson
import           Data.Text

import           Database.Groundhog                  as GH
import           Database.Groundhog.Core             (PersistField)
import           Database.Groundhog.TH
import           Database.Groundhog.Utils.Postgresql (keyToInt)

import           GHC.Generics

import           Logging                             (Priority (INFO))
import           NoteScript                          as NS
import           Prosper                             hiding (Money)

import           PeerTrader.Database
import           PeerTrader.Types                    (UserLogin)

data Strategy a = Strategy
    { name         :: !Text
    , owner        :: !UserLogin
    , amount       :: !Money
    , riskSettings :: !RiskSettings
    , strategy     :: !a
    } deriving (Show, Eq, Generic)

instance Functor Strategy where
    fmap f s = s { strategy = f (strategy s) }

data RiskSettings = RiskSettings
    { stopAfter :: !(Maybe Money)
    , dailyMax  :: !(Maybe Money)
    } deriving (Show, Eq, Generic)

mkPersist defaultCodegenConfig [groundhog|
embedded: RiskSettings
|]

mkPersist defaultCodegenConfig [groundhog|
entity: Strategy
constructors:
  - name: Strategy
    fields:
      - name: owner
        type: text
        reference:
          table: snap_auth_user
          columns: [login]
      - name: amount
        type: double precision
|]

instance ToJSON a => ToJSON (Strategy a) where
instance FromJSON a => FromJSON (Strategy a) where

instance ToJSON RiskSettings where
instance FromJSON RiskSettings where

data InvestState = InvestState
    { amountInvested :: !Money
    , dailyInvested  :: !Money
    } deriving (Show, Eq, Generic)

instance ToJSON InvestState where

newInvestment :: Money -> InvestState -> InvestState
newInvestment amt (InvestState invested daily) = InvestState (invested + amt) (daily + amt)

resetDaily :: InvestState -> InvestState
resetDaily (InvestState invested _) = InvestState invested 0

mkPersist defaultCodegenConfig [groundhog|
entity: InvestState
|]

emptyInvestState :: InvestState
emptyInvestState = InvestState 0 0

runStrategy
    :: (a -> NoteScript Listing ir Bool)
    -> TVar (Entity InvestState)
    -> Strategy a
    -> NoteScript Listing ir (Maybe (Async ir))
runStrategy eval tInvestState (Strategy _ _ amt risk s) = do
    Entity _ investState <- runIO $ readTVarIO tInvestState
    manageRisk amt risk investState $ do
        result <- eval s
        if result
            then do
                sid <- NS.get listingId
                logS INFO $ "Investing " ++ show amt ++ " in loan " ++ show sid

                resp <- NS.invest amt
                return $ Just resp
            else return Nothing

manageRisk
    :: Money
    -> RiskSettings
    -> InvestState
    -> NoteScript l ir (Maybe (Async ir))
    -> NoteScript l ir (Maybe (Async ir))
manageRisk amt (RiskSettings (Just totalMax) (Just dayMax)) (InvestState invested daily) k =
    if invested + amt <= totalMax || daily + amt <= dayMax then k else return Nothing
manageRisk amt (RiskSettings (Just totalMax) Nothing) (InvestState invested _) k =
    if invested + amt <= totalMax then k else return Nothing
manageRisk amt (RiskSettings Nothing (Just dayMax)) (InvestState _ daily) k =
    if daily + amt <= dayMax then k else return Nothing
manageRisk _ (RiskSettings Nothing Nothing) _ k = k

data StrategyState a = StrategyState
    { strategyRef    :: !(DefaultKey (Strategy a))
    , investStateRef :: !(DefaultKey InvestState)
    }

mkPersist defaultCodegenConfig [groundhog|
entity: StrategyState
|]

getInvestState
    :: (PersistBackend m, PersistField a)
    => DefaultKey (Strategy a)
    -> m (Maybe (Entity InvestState))
getInvestState stratKey = do
    states <- project InvestStateRefField $ StrategyRefField ==. stratKey
    case states of
        stateKey:_ -> do
            mState <- GH.get stateKey
            case mState of
                Just state -> return $ Just $ Entity (keyToInt stateKey) state
                _ -> return Nothing
        _ -> return Nothing
