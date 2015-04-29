{-# LANGUAGE OverloadedStrings #-}

module P2PPicks.Request
    ( subscriberStatus
    , validateUser
    , reportInvestment
    , APIKey
    , APISecret
    , SubscriberID
    ) where

import           Network.Http.Client
import           OpenSSL

import           Crypto.Hash.MD5        (hash)

import           Control.Lens           (re, (^?))
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString        hiding (map)
import qualified Data.ByteString.Base16 as B
import qualified Data.ByteString.Lazy   as L
import           Data.List              (intersperse)
import           Data.Monoid            (mconcat, (<>))
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8)
import           Data.Text.Strict.Lens  (utf8)

import           Logging                (debugM)

import           P2PPicks.Types

signRequest
    :: ByteString -- ^ Method-action
    -> [(ByteString, ByteString)] -- ^ Parameters, must have secret last
    -> ByteString -- ^ Resulting MD5 sig
signRequest action params =
    B.encode . hash . mconcat . Data.List.intersperse "&" $
        action : map (uncurry (<>)) params

issueRequest :: (FromJSON a, MonadIO m)
    => ByteString -- ^ API Endpoint
    -> ByteString -- ^ Action
    -> [(ByteString, ByteString)] -- ^ Params
    -> m a -- ^ JSON Result
issueRequest endpoint action params = liftIO . withOpenSSL $ do
    ctx <- baselineContextSSL
    con <- openConnectionSSL ctx "www.p2p-picks.com" 443
    req <- buildRequest $ do
        http POST $ "/api/v1/" <> endpoint
        setContentType "application/x-www-form-urlencoded"
    sendRequest con req (encodedFormBody request)
    resp <- receiveResponse con jsonHandler
    closeConnection con
    return resp
  where
    request = params ++ [("sig", signRequest action params)]

type APIKey = ByteString
type APISecret = ByteString
type SubscriberID = ByteString

statusSuccess :: Value -> Bool
statusSuccess result =
    result ^? key "meta".key "status"._Number == Just 200

responseActive :: Value -> Bool
responseActive result =
    result ^? key "response".key "status"._String == Just "active"

subscriberStatus
    :: MonadIO m
    => APIKey -- ^ API Key
    -> APISecret -- ^ API Secret
    -> SubscriberID -- ^ Subscriber ID
    -> m Bool -- ^ Active or Inactive, returns False if the request failed
subscriberStatus apiKey apiSecret subscriberId = do
    result <- issueRequest "subscriber/status" "subscriber-status"
        [ ("api_key", apiKey)
        , ("p2p_sid", subscriberId)
        , ("secret", apiSecret) ]
    return (statusSuccess result && responseActive result)

validateUser
    :: MonadIO m
    => APIKey
    -> APISecret
    -> ByteString -- ^ Email address of user
    -> ByteString -- ^ Password of user
    -> m (Maybe (SubscriberID, Bool)) -- ^ (SubscriberID, Status)
validateUser apiKey apiSecret email password = do
    result <- issueRequest "subscriber/validate" "subscriber-validate"
        [ ("api_key", apiKey)
        , ("p2p_email", email)
        , ("p2p_password", password)
        , ("secret", apiSecret) ]
    return $ if statusSuccess result
        then
            let status = key "response".key "status"._String
                isActive = result ^? status == Just "active"
                sid = result ^? key "response".key "sid"._String.re utf8
            in maybe Nothing (\s -> Just (s, isActive)) sid
        else Nothing

reportInvestment
    :: MonadIO m
    => APIKey
    -> APISecret
    -> SubscriberID
    -> P2PPicksType
    -> Int -- ^ Listing ID
    -> Double -- ^ Amount
    -> m Bool -- ^ Success?
reportInvestment apiKey apiSecret subscriberId picksType listingId amt = do
    result <- issueRequest "subscriber/report" "subscriber-report"
        [ ("api_key", apiKey)
        , ("p2p_payload", L.toStrict $ encode p2pPayload)
        , ("secret", apiSecret) ]
    debugM "P2PPicks" $
        show (encode p2pPayload)
        ++ "\n" ++
        show result
    return (statusSuccess result)
  where
    productType :: P2PPicksType -> Text
    productType ProfitMax = "prosper-profit-maximizer"
    productType LossMin = "prosper-loss-minimizer"

    p2pPayload = [ object
        [ "sid" .= decodeUtf8 subscriberId
        , "picks" .= [ object
            [ "product" .= productType picksType
            , "loan_id" .= listingId
            , "note" .= amt
            ]]
        ]]
