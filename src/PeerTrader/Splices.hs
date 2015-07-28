{-# LANGUAGE OverloadedStrings #-}

module PeerTrader.Splices where

import           Control.Monad                 (liftM)
import           Control.Monad.Trans           (lift)
import           Control.Monad.Util            (ifM)

import qualified Data.Text                     as T
import           Text.XmlHtml                  as X

import           Heist
import           Snap.Extras.CoreUtils         (finishEarly)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple

import           Application

-- | If the user is an admin, then display what's in
-- > <ifAdmin> example </ifAdmin>
ifAdmin :: SnapletISplice App
ifAdmin = ifM (lift isAdmin)
    (X.childNodes `liftM` getParamNode)
    (return [])

newtype Login = L T.Text

instance FromRow Login where
    fromRow = L <$> field

-- | Helper function for checking if the current user has the "admin" role.
isAdmin :: AppHandler Bool
isAdmin = withCurrentUser $ \n -> do
    admins <- query isAdminQuery (Only n)
    return . not . null $ (admins :: [Login])

isAdminQuery :: Query
isAdminQuery = "SELECT * FROM peertrader_admins WHERE login=?"

withAdminUser :: AppHandler () -> AppHandler ()
withAdminUser f = ifM isAdmin f (finishEarly 403 "Not admin user!")
