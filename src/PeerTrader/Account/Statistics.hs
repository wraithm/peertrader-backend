{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Account.Statistics where

import           Snap.Extras                    (finishEarly)
import           Snap.Snaplet                   (with)
import           Snap.Snaplet.Auth              (AuthUser (..), currentUser)

import           Control.Applicative            ((<$>), (<*>))

import           Data.Function                  (on)

import           Database.Groundhog

import           Prosper.Invest                 (InvestMessage (..),
                                                 InvestStatus (..))

import           Application                    (AppHandler, auth, runGH,
                                                 withCurrentUser)
import           PeerTrader.Investment.Database
import           PeerTrader.Prosper.Listing     as L

floatDiv :: Int -> Int -> Double
floatDiv = (/) `on` fromIntegral

-- | 'hitRate' is the number of times that your strategy made an investment attempt
-- on a strategy.
hitRate :: AppHandler Double
hitRate = do
    (n, createdAt) <- with auth currentUser >>= dateCreated

    runGH $ floatDiv <$> numInvests n <*> numListingsSince createdAt
  where
    dateCreated (Just AuthUser
        { userCreatedAt = Just createdDate
        , userLogin = n }) = return (n, createdDate)
    dateCreated _ = finishEarly 401 "Not logged in!"

    numInvests n = count $ OwnerField ==. n
    numListingsSince date = count $ L.TimeStampField >=. date

successRate :: AppHandler Double
successRate = withCurrentUser $ \n ->
    runGH $ ((/) `on` sum) <$> numSuccess n <*> numListings n
  where
    numSuccess n = project (InvestmentField ~> AmountInvestedSelector) $
        OwnerField ==. n &&.
        InvestmentField ~> InvestStatusSelector ==. Success
    numListings n = project (InvestmentField ~> RequestedAmountSelector) $
        OwnerField ==. n &&.
        (InvestmentField ~> InvestMessageSelector) /=. InvestedAmountLessThanMinimumRequired &&.
        (InvestmentField ~> InvestMessageSelector) /=. InsufficientFunds
