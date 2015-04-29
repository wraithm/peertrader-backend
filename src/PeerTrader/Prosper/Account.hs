{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module PeerTrader.Prosper.Account where

import           Control.Concurrent.STM
import           Control.Monad.Reader

import           Data.Aeson
import           Data.HashMap.Strict    as H
import           Data.Time.Clock

import           Database.Groundhog
import           Database.Groundhog.TH

import           Prosper.Account

import           Application
import           PeerTrader.Account.Web
import           PeerTrader.Database    (runDb)
import           PeerTrader.Types       (UserLogin)

data ProsperAccount = ProsperAccount
    { owner     :: !UserLogin
    , timeStamp :: !UTCTime
    , account   :: !Account
    }

instance ToJSON ProsperAccount where
    toJSON (ProsperAccount _ t a) = object
        [ "timeStamp" .= t
        , "account" .= a
        ]

saveAccounts :: AppReader ()
saveAccounts = do
    accts <- liftIO . readTVarIO =<< asks _accounts
    _ <- H.traverseWithKey saveAccount accts
    return ()

saveAccount :: UserLogin -> AccountData -> AppReader ()
saveAccount n (AccountData { _prosperAccount = tAcct }) = do
    acct <- liftIO $ readTVarIO tAcct
    when (acct /= emptyAccount) $ do
        now <- liftIO getCurrentTime
        runDb $ insert_ $ ProsperAccount n now acct

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - embedded: Account
    fields:
      - name: availableCash
        type: double precision
      - name: pendingInvestments
        type: double precision
      - name: totPrincipalRecvdOnActiveNotes
        type: double precision
      - name: totalInvestedOnActiveNotes
        type: double precision
      - name: outstandingPrincipalActiveNotes
        type: double precision
      - name: totalAccountValue
        type: double precision
  - entity: ProsperAccount
    constructors:
      - name: ProsperAccount
        fields:
          - name: owner
            type: text
            reference:
              table: snap_auth_user
              columns: [login]
          - name: timeStamp
            type: timestamptz
            default: now()
|]

latestProsperAccount :: PersistBackend m => UserLogin -> m Account
latestProsperAccount n = do
    pAccts <- select $
        (OwnerField ==. n) `orderBy` [Desc TimeStampField] `limitTo` 1
    return $ case pAccts of
        ProsperAccount _ _ acct:_ -> acct
        _ -> emptyAccount

accountTimeSeries :: PersistBackend m => UserLogin -> m [ProsperAccount]
accountTimeSeries n = select (OwnerField ==. n)
