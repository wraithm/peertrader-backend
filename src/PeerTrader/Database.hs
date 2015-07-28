{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module PeerTrader.Database where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control

import           Data.Aeson
import qualified Data.HashMap.Strict                 as H
import qualified Data.Vector                         as V

import           Database.Groundhog.Core
import           Database.Groundhog.Generic
import qualified Database.Groundhog.Postgresql.Array as P
import           Database.Groundhog.Utils.Postgresql (intToKey, keyToInt)

instance ToJSON a => ToJSON (P.Array a) where
    toJSON (P.Array x) = toJSON x

instance FromJSON a => FromJSON (P.Array a) where
    parseJSON (Array x) = fmap P.Array $ mapM parseJSON $ V.toList x
    parseJSON _ = return $ P.Array []

data Entity a = Entity
    { key    :: !Int
    , entity :: !a
    } deriving (Show, Eq)

instance Functor Entity where
    fmap f (Entity k v) = Entity k (f v)

selectEntity :: (PersistBackend m,
                 Projection constr b,
                 ProjectionDb constr (PhantomDb m),
                 ProjectionRestriction constr (RestrictionHolder v c),
                 HasSelectOptions opts (PhantomDb m) (RestrictionHolder v c),
                 EntityConstr v c,
                 AutoKey v ~ Key b a,
                 PrimitivePersistField (Key b a))
    => constr
    -- ^ The constructor type for the object being queried
    -> opts
    -- ^ Same as the opts argument to groundhog's select funciton
    -> m [Entity b]
selectEntity constructor cond = do
    res <- project (AutoKeyField, constructor) cond
    return $ map (\(k, v) -> Entity (keyToInt k) v) res

replaceEntity :: (PersistEntity a,
                  PersistBackend m,
                  PrimitivePersistField (Key a BackendSpecific))
    => Entity a
    -> m ()
replaceEntity (Entity k v) = replace (intToKey k) v

instance ToJSON a => ToJSON (Entity a) where
    toJSON (Entity k e) = case toJSON e of
        Object x -> Object $ H.insert "id" (toJSON k) x
        x -> x

instance FromJSON a => FromJSON (Entity a) where
    parseJSON o@(Object x) = Entity <$> x .: "id" <*> parseJSON o
    parseJSON x = fail $ "Could not parse Entity JSON: " ++ show x

runDb :: (ConnectionManager cm conn, MonadBaseControl IO m, MonadIO m, MonadReader cm m) =>
    DbPersist conn (NoLoggingT m) a -> m a
runDb f = ask >>= runDbConn f
