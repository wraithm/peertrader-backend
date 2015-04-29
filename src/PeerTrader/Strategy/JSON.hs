{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Strategy.JSON (addOwnerValidate) where

import           Snap.Extras.CoreUtils        (finishEarly)
import           Snap.Extras.JSON             (reqJSON)

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict          as H

import           Application                  (AppHandler)

import           PeerTrader.Strategy.Strategy
import           PeerTrader.Types             (UserLogin)

-- | Add the owner field to a Strategy JSON object, and validate for sane input
addOwnerValidate :: FromJSON a => UserLogin -> AppHandler a
addOwnerValidate n = do
    Object obj <- reqJSON

    -- Check amount
    case H.lookup "amount" obj of
        Just (Number x)
            | x < 25 -> -- 25 for the Prosper requirement...
                badReq "Amount must be greater than $25.00."
            | otherwise -> return ()
        Nothing -> badReq "Amount required."
        Just Null -> badReq "Amount required."
        _ -> badReq "Amount must be a number."

    -- If RiskSettings aren't present, set them to empty
    let emptyRiskSettings = toJSON $ RiskSettings Nothing Nothing
        obj' = case H.lookup "riskSettings" obj of
                Nothing -> H.insert "riskSettings" emptyRiskSettings obj
                _ -> obj

    -- Add owner
    let strat = Object $ H.insert "owner" (toJSON n) obj'
        Just strat' = parseMaybe parseJSON strat

    return strat'
  where
    badReq = finishEarly 400

