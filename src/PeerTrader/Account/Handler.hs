module PeerTrader.Account.Handler where

import           Control.Monad                      (liftM)

import           Data.Maybe                         (listToMaybe)
import           Data.Text              (Text)

import           Database.Groundhog

import           Prosper

import           Application
import           PeerTrader.Account.Account
import           PeerTrader.Types
import PeerTrader.Account.Web

newPeerTraderAccount :: UserLogin -> Text -> AppHandler ()
newPeerTraderAccount n s = runGH $
    insert_ $ PeerTraderAccount n False Nothing s False

setEnabledProsperAccount :: UserLogin -> Bool -> AppHandler ()
setEnabledProsperAccount n b = runGH $
    update [ProsperEnabledField =. b] $ LoginField ==. n

getCheckTerms :: UserLogin -> AppHandler (Maybe Bool)
getCheckTerms n = runGH $ liftM listToMaybe $
    project CheckTermsField (LoginField ==. n)

setCheckTerms :: UserLogin -> Bool -> AppHandler ()
setCheckTerms n b = runGH $
    update [CheckTermsField =. b] (LoginField ==. n)

updateAccount :: UserLogin -> AccountData -> AppHandler ()
updateAccount n (AccountData { _prosperUser = ui }) = runGH $ do
    uiKey <- insert ui
    update [ProsperUserKeyField =. Just uiKey] $ LoginField ==. n

deleteAccount :: UserLogin -> AppHandler ()
deleteAccount n = runGH $
    update [ProsperUserKeyField =. emptyUi] $ LoginField ==. n
  where
    emptyUi :: Maybe (DefaultKey User)
    emptyUi = Nothing

accountStateProsperEnabled :: UserLogin -> AppHandler (Maybe (Text, Bool))
accountStateProsperEnabled n = runGH $ liftM listToMaybe $
    project (StateField, ProsperEnabledField) (LoginField ==. n)
