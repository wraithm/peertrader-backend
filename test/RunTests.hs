{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- For cabal build on Jenkins
-- import Test.Tasty.Runners.AntXML

import PeerTrader.AutoFilter.AutoFilter
import PeerTrader.AutoFilter.Range
import Prosper
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict as H
import Control.Applicative

instance Arbitrary ProsperRating where
    arbitrary = elements [AA, A, B, C, D, E, HR]

instance Arbitrary ProsperCategory where
    arbitrary = elements 
        [ NotAvailable
        , DebtConsolidation
        , HomeImprovement
        , Business
        , PersonalLoan
        , StudentUse
        , Auto
        , Other
        , BabyAdoptionLoans
        , Boat
        , CosmeticProcedures
        , EngagementRingFinancing
        , GreenLoans
        , HouseholdExpenses
        , LargePurchases
        , MedicalDental
        , Motorcycle
        , RV
        , Taxes
        , Vacation
        , WeddingLoans
        ]

instance Arbitrary a => Arbitrary (Range a) where
    arbitrary = Range <$> arbitrary <*> arbitrary

instance Arbitrary AutoFilter where
    arbitrary = AutoFilter
        <$> pure "lel"
        <*> pure "lol"
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

testAddOwnerCheckAmount :: AutoFilter -> AutoFilter
testAddOwnerCheckAmount af = af''
  where
    Just (Object obj) = (decode . encode) af
    Just amt = H.lookup "amount" obj
    amt' = if amt == Null then Number 25.0 else amt
    af' = Object $ H.insert "amount" amt' obj
    Just af'' = parseMaybe parseJSON af'

main :: IO ()
-- main = defaultMainWithIngredients [ antXMLRunner ] $
main = defaultMain $
    testGroup "Tests"
        [ testGroup "First test group" 
            [ testCase "lol2" $ assertEqual "lol2 test" (1 :: Integer) 1 ]
        , testGroup "peertrader test group"
            [ testCase "kernel test the other" $ assertEqual "the test 2 of 1" (1 :: Integer) 1 ]
        , testGroup "AutoFilter validation"
            [ testProperty "Amount isn't NaN" $ \af -> 
                not (isNaN (amount (testAddOwnerCheckAmount af)))
            ]
        ]
