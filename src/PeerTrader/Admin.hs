{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PeerTrader.Admin
    ( adminJSON
    , peertraderUsers
    , mostRecentStats
    ) where

import           Snap.Extras                                 (writeJSON)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple ()
import           Snap.Snaplet.PostgresqlSimple

import           Data.Aeson
import           Data.Maybe                                  (listToMaybe)
import           Data.Text                                   (Text)
import           Data.Time.Clock

import           Database.Groundhog

import           GHC.Generics

import           Application
import           PeerTrader.Account.Handler                  (accountStateProsperEnabled)
import           PeerTrader.Admin.Database                   as A
import           PeerTrader.Investment.Database              as I
import           PeerTrader.Splices

selectAllAuthUsers :: Query
selectAllAuthUsers = "SELECT * FROM snap_auth_user"

countAutoFilters :: Query
countAutoFilters = "SELECT count(*) FROM \"Strategy#AutoFilter\" WHERE owner=?"

data PeerTraderUser = User
    { uid            :: !(Maybe UserId)
    , login          :: !Text
    , loginCount     :: !Int
    , createdAt      :: !(Maybe UTCTime)
    , lastLoginAt    :: !(Maybe UTCTime)
    , numAutoFilters :: !Int
    , numInvestments :: !Int
    , prosperEnabled :: !Bool
    , state          :: !Text
    } deriving (Generic, Show)

instance ToJSON PeerTraderUser where
instance FromJSON PeerTraderUser where

peerTraderUser :: AuthUser -> AppHandler PeerTraderUser
peerTraderUser AuthUser{..} = do
    Only numAFs:_ <- with db $ query countAutoFilters (Only userLogin)
    -- TODO The following query fails (pattern match) if the record doesn't exist...
    Just (s,pe) <- accountStateProsperEnabled userLogin
    numInvsts <- runGH $ count (I.OwnerField ==. userLogin)
    return User
        { uid = userId
        , login = userLogin
        , loginCount = userLoginCount
        , createdAt = userCreatedAt
        , lastLoginAt = userCurrentLoginAt
        , numAutoFilters = numAFs
        , numInvestments = numInvsts
        , prosperEnabled = pe
        , state = s
        }

peertraderUsers :: AppHandler [PeerTraderUser]
peertraderUsers = do
    users <- with db $ query_ selectAllAuthUsers
    mapM peerTraderUser users

mostRecentStats :: AppHandler (Maybe Statistics)
mostRecentStats = do
    stats <- runGH $ select $ CondEmpty `orderBy` [Desc A.TimeStampField] `limitTo` 1
    return $ listToMaybe stats

adminJSON :: ToJSON a => AppHandler a -> AppHandler ()
adminJSON f = withAdminUser (f >>= writeJSON)
