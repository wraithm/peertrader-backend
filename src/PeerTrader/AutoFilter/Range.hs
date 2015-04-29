{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module PeerTrader.AutoFilter.Range
    ( Range (..) )
    where

import           Database.Groundhog.Core
import           Database.Groundhog.Generic
import           Database.PostgreSQL.Simple.FromField hiding (Range, name)
import           Database.PostgreSQL.Simple.ToField

import           Control.Applicative                  (pure, (<$>), (<*>))
import           Control.Monad                        (mzero)

import           Blaze.ByteString.Builder             (fromByteString)
import           Data.Aeson
import           Data.Attoparsec.ByteString.Char8     hiding (D)
import           Data.ByteString.Char8
import           Data.Typeable

-- | Range of values
data Range a = Range
    { low  :: !a -- ^ The low value of the range
    , high :: !a -- ^ The high value of the range
    } deriving (Show, Eq, Typeable)

instance PersistField (Range Int) where
    persistName _ = "Range"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef [Left "int4range"]) False Nothing Nothing

instance PersistField (Range Double) where
    persistName _ = "Range"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive (DbOther $ OtherTypeDef [Left "numrange"]) False Nothing Nothing

instance PrimitivePersistField (Range Int) where
    toPrimitivePersistValue _ (Range x y) = PersistString $ "[" ++ show x ++ "," ++ show y ++ "]"
    fromPrimitivePersistValue _ (PersistString a) =
        case parseOnly parseIntegralRange bs of
            Left _ -> Range 0 0
            Right b -> b
      where bs = pack a
    fromPrimitivePersistValue _ (PersistByteString a) =
        case parseOnly parseIntegralRange a of
            Left _ -> Range 0 0
            Right b -> b
    fromPrimitivePersistValue _ _ = Range 0 0

instance PrimitivePersistField (Range Double) where
    toPrimitivePersistValue _ (Range x y) = PersistString $ "[" ++ show x ++ "," ++ show y ++ "]"
    fromPrimitivePersistValue _ (PersistString a) =
        case parseOnly parseDoubleRange bs of
            Left _ -> Range 0 0
            Right b -> b
      where bs = pack a
    fromPrimitivePersistValue _ (PersistByteString a) =
        case parseOnly parseDoubleRange a of
            Left _ -> Range 0 0
            Right b -> b
    fromPrimitivePersistValue _ _ = Range 0 0

instance NeverNull (Range Int) where
instance NeverNull (Range Double) where

parseRange :: Num a => Parser a -> Parser (Range a)
parseRange p = do
    _ <- char '['
    x <- p
    _ <- char ','
    y <- p
    c <- peekChar' -- WARNING, this will fail if end of input
    return $ case c of
        ')' -> Range x (y - 1)
        ']' -> Range x y
        _ -> Range x y

parseDoubleRange :: Parser (Range Double)
parseDoubleRange = parseRange double

parseIntegralRange :: Integral a => Parser (Range a)
parseIntegralRange = parseRange decimal

-- | Helper for ToField instances
lit :: ByteString -> Action
lit = Plain . fromByteString

-- Write tests to make sure that toField . fromField and fromField . toField produce the same result
instance ToField a => ToField (Range a) where
    toField (Range {..}) = Many [lit "'[", toField low, lit ",", toField high, lit "]'"]

instance FromField (Range Double) where
    fromField f Nothing = returnError ConversionFailed f "Range Double couldn't be parsed."
    fromField f (Just bs) = case parseOnly parseDoubleRange bs of
        Left err -> returnError ConversionFailed f err
        Right a -> pure a

instance FromField (Range Int) where
    fromField f Nothing = returnError ConversionFailed f "Range Integral couldn't be parsed."
    fromField f (Just bs) = case parseOnly parseIntegralRange bs of
        Left err -> returnError ConversionFailed f err
        Right a -> pure a

-- TODO Create tests to say that toJSON . parseJSON and parseJSON . toJSON produce the same result
-- TODO Check if generic instances work in this case
instance ToJSON a => ToJSON (Range a) where
    toJSON (Range {..}) = object
        [ "low" .= low
        , "high" .= high
        ]

instance FromJSON a => FromJSON (Range a) where
    parseJSON (Object v) = Range
        <$> v .: "low"
        <*> v .: "high"
    parseJSON _ = mzero
