{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module P2PPicks
    ( P2PPicks (..)
    , initializeP2PPicks
    , module R
    , module T
    ) where

import           Data.Configurator       as C
import           Data.Configurator.Types

import           System.FilePath         ((</>))

import           P2PPicks.Request        as R
import           P2PPicks.Types          as T

data P2PPicks = P2PPicks
    { guardian         :: FilePath

    , profitMaxLicense :: FilePath
    , lossMinLicense   :: FilePath

    , paramM           :: String
    , paramB           :: String
    } deriving (Eq, Show)

initializeP2PPicks :: FilePath -> Config -> IO P2PPicks
initializeP2PPicks snapletFilePath config = do
    guardian <- (snapletFilePath </>) <$> lookupDefault defaultGuardian config "p2ppicks.guardian"

    profitMaxLicense <- (snapletFilePath </>) <$> lookupDefault defaultProfitMaxLicense config "p2ppicks.profitMaxLicense"
    lossMinLicense <- (snapletFilePath </>) <$> lookupDefault defaultLossMinLicense config "p2ppicks.profitMaxLicense"

    paramM <- lookupDefault defaultParamM config "p2ppicks.paramM"
    paramB <- lookupDefault defaultParamB config "p2ppicks.paramB"

    return P2PPicks {..}
  where
    defaultGuardian = "guardian_linux"
    defaultProfitMaxLicense = "PT_Prosper_PMax_hex"
    defaultLossMinLicense = "PT_Prosper_LMin_hex"

    defaultParamM = "0"
    defaultParamB = "0"
