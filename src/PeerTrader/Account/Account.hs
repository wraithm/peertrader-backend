{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module PeerTrader.Account.Account where

import           Data.Text              (Text)

import           Database.Groundhog
import           Database.Groundhog.TH

import           Prosper.User

import           PeerTrader.Types

data PeerTraderAccount = PeerTraderAccount
    { login          :: !UserLogin -- ^ References snap_auth_user
    , prosperEnabled :: !Bool
    , prosperUserKey :: !(Maybe (DefaultKey User))
    , state          :: !Text
    , checkTerms     :: !Bool
    }

mkPersist defaultCodegenConfig [groundhog|
- entity: User
- entity: PeerTraderAccount
  constructors:
    - name: PeerTraderAccount
      fields:
        - name: login
          type: text
          reference:
            table: snap_auth_user
            columns: [login]
        - name: state
          type: text
|]
