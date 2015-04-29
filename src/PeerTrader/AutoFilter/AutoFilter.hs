{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}

module PeerTrader.AutoFilter.AutoFilter
    ( AutoFilter (..)
    , autoFilter
    , AutoFilterConstructor (..)
    ) where

import           Control.Monad               (guard, mzero, unless)
import           Control.Monad.Trans         (lift)
import           Control.Monad.Trans.Maybe

import           Data.Aeson
import           GHC.Generics

import           Database.Groundhog          ()
import           Database.Groundhog.TH

import           NoteScript                  as N
import           Prosper                     hiding (Money)

import           PeerTrader.AutoFilter.Range

-- | TODO Needs to be abstracted to multiple backends or something
data AutoFilter = AutoFilter
    { -- Filters
      ratings                       :: ![Rating]
    , categories                    :: ![Category]
    , ficoRange                     :: !(Maybe (Range Int))
    , bankcardUtilizationRange      :: !(Maybe (Range Double))
    , -- | Nothing represents No filter.
      -- Just True is Only homeowners. Just False is only non-homeowners.
      isHomeownerFilter             :: !(Maybe Bool)
    , yieldRange                    :: !(Maybe (Range Double))
    , effectiveYieldRange           :: !(Maybe (Range Double))
    , rateRange                     :: !(Maybe (Range Double))
    , aprRange                      :: !(Maybe (Range Double))
    , -- | Similar to how isHomeownerFilter works.
      -- Just True means that you only want 3 year terms
      -- Just False is for only 5 year terms
      -- Nothing is for both
      termInMonthsFilter            :: !(Maybe Bool)
    , incomeRangeRange              :: !(Maybe (Range Money))
    , statedMonthlyIncomeRange      :: !(Maybe (Range Money))
    , debtToIncomeRange             :: !(Maybe (Range Double))
    , amountDelinquentRange         :: !(Maybe (Range Money))
    , openCreditLinesRange          :: !(Maybe (Range Int))
    , totOpenRevolvingAcctsRange    :: !(Maybe (Range Int))
    , revolvingBalanceRange         :: !(Maybe (Range Money))
    , revolvingAvailableCreditRange :: !(Maybe (Range Int)) -- ^ Percent
    , nowDelinquentDerogRange       :: !(Maybe (Range Int))
    , wasDelinquentDerogRange       :: !(Maybe (Range Int))
    } deriving (Show, Eq, Generic)

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - primitive: Rating
  - primitive: Category
|]

mkPersist defaultCodegenConfig [groundhog|
entity: AutoFilter
|]

instance ToJSON AutoFilter where
instance FromJSON AutoFilter where

-- | Check if a value is inside of a range, inclusively
inclInRange :: Ord a => a -> Range a -> Bool
inclInRange x (Range l h) = x >= l && x <= h

-- | Check if a value from a Listing is inside of a range, if not, then do not
-- continue with the investment in the 'autoFilter' function. This can be thought
-- of as a guard on a range.
guardInRange :: Ord a
    => (Listing -> a) -- ^ Accessor function for the value in the listing
    -> Maybe (Range a) -- ^ Range considered
    -> MaybeT ProsperScript ()
guardInRange _ Nothing = return ()
guardInRange x (Just range) = do
    x' <- lift $ get x
    guard (inclInRange x' range)

-- | If the field is not included in the listing, then fall through, else do
-- a normal check.
--
-- TODO This should probably check if the range is well defined. Perhaps a
-- Maybe (Range a) makes sense. Nothing would mean that we just pass-through
-- and ignore the condition. Just (Range) means that the field in the listing
-- must be defined and that it must be in the appropriate range.
guardMaybeInRange
    :: Ord a
    => (Listing -> Maybe a)
    -> Maybe (Range a)
    -> MaybeT ProsperScript ()
guardMaybeInRange _ Nothing = return ()
guardMaybeInRange x (Just range) = do
    x' <- lift $ get x
    case x' of
        Nothing -> return ()
        Just x'' -> guard (inclInRange x'' range)

-- | This guard handles boolean values from listings with the option of handling
-- both cases. The encoding is:
-- Nothing means accept both
-- Just True means that the listing property must have a value of True
-- Just False means that the listing property must be False
guardMaybeBool :: (Listing -> Bool) -> Maybe Bool -> MaybeT ProsperScript ()
guardMaybeBool _ Nothing = return ()
guardMaybeBool x (Just y) = do
    x' <- lift $ get x
    unless (x' == y) mzero

-- | A guard for 'incomeRange'. If both ends of the income range are in the
-- given range for the strategy, then pass-through, else 'mzero'.
guardIncomeRange :: Maybe (Range Money) -> MaybeT ProsperScript ()
guardIncomeRange Nothing = return ()
guardIncomeRange (Just range) = do
    x <- lift $ get (incomeRange . credit)
    case x of
        -- TODO Might have to add another filter for employment
        -- Check if the investor is willing to invest in unemployed
        Nothing -> return () -- If they're unemployed, then just continue
        Just (x', y') -> unless (inclInRange x' range && inclInRange y' range) mzero

-- | Convert a 'AutoFilter' into a 'NoteScript'
-- Invest if all of the guards pass
autoFilter :: AutoFilter -> ProsperScript Bool
autoFilter AutoFilter{..} = fmap maybeToBool . runMaybeT $ do
    r <- lift $ get rating
    guard (null ratings || r `elem` ratings) -- If the ratings are [], then ignore.
    c <- lift $ get category
    guard (null categories || c `elem` categories)

    guardInRange (yield . offer) yieldRange
    guardInRange (effectiveYield . offer) effectiveYieldRange
    guardInRange (apr . offer) aprRange

    guardInRange (rate . offer) rateRange
    guardMaybeBool ((== 36) . termInMonths . offer) termInMonthsFilter

    guardInRange (fico . credit) ficoRange
    guardInRange (bankcardUtilization . credit) bankcardUtilizationRange
    guardMaybeBool (isHomeowner . credit) isHomeownerFilter
    guardInRange (debtToIncome . credit) debtToIncomeRange
    guardIncomeRange incomeRangeRange
    guardMaybeInRange (statedMonthlyIncome . credit) statedMonthlyIncomeRange
    guardMaybeInRange (amountDelinquent . credit) amountDelinquentRange
    guardMaybeInRange (openCreditLines . credit) openCreditLinesRange
    guardMaybeInRange (totOpenRevolvingAccts . credit) totOpenRevolvingAcctsRange
    guardMaybeInRange (revolvingBalance . credit) revolvingBalanceRange
    guardMaybeInRange (revolvingAvailableCredit . credit)  revolvingAvailableCreditRange
    guardMaybeInRange (nowDelinquentDerog . credit) nowDelinquentDerogRange
    guardMaybeInRange (wasDelinquentDerog . credit) wasDelinquentDerogRange
  where
    maybeToBool :: Maybe () -> Bool
    maybeToBool (Just _) = True
    maybeToBool Nothing = False
