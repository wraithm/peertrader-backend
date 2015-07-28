{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

import           Control.Concurrent                           (forkIO)
import           Control.Concurrent.STM
import           Control.Exception                            (Handler (..),
                                                               SomeException,
                                                               catches)
import           Control.Monad                                (foldM)
import           Control.Monad.IO.Class                       (liftIO)


import           Data.ByteString.Char8                        as B
import           Data.Configurator                            as C
import           Data.HashMap.Strict                          as H
import           Data.Pool

import           Database.Groundhog.Postgresql

import           Heist.Interpreted
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Ekg                             (ekgInit)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.P2PPicks
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.SES
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

import           Logging
import           P2PPicks.Types                               (P2PPicksType)
import           Prosper                                      as P
import           Prosper.Monad

import           Application
import           PeerTrader
import           PeerTrader.Account.Account
import           PeerTrader.Account.Web
import           PeerTrader.Admin
import           PeerTrader.Admin.Database
import           PeerTrader.Admin.Statistics
import           PeerTrader.AutoFilter.AutoFilter
import           PeerTrader.Investment.Database
import           PeerTrader.NewUser                           (NewUser)
import           PeerTrader.P2PPicks.Account                  (P2PPicksAccount, getP2PPicksAccount)
import           PeerTrader.P2PPicks.Result                   (P2PPicksResult)
import           PeerTrader.Prosper.Account                   as A
import           PeerTrader.Prosper.Listing
import           PeerTrader.Schedule.Web
import           PeerTrader.Socket.Activity
import           PeerTrader.Socket.Web
import           PeerTrader.Splices
import           PeerTrader.Strategy.Strategy
import           PeerTrader.StrategyManagement.ActiveStrategy
import           PeerTrader.Types

import           PeerTrader.Route.AutoFilter
import           PeerTrader.Route.P2PPicks
import           PeerTrader.Route.P2PPicksAccount
import           PeerTrader.Route.ProsperAccount
import           PeerTrader.Route.Statistics
import           PeerTrader.Route.StrategyManagement
import           PeerTrader.Route.StreamData
import           PeerTrader.Route.User

initializeDatabase
    :: Pool Postgresql
    -> IO (TVar (HashMap UserLogin AccountData))
initializeDatabase g = do
    ptUsers <- runDbConn selectUser g
    hm <- foldM newAccount H.empty ptUsers
    newTVarIO hm
  where
    selectUser = select $ ProsperEnabledField ==. True

    maybe' :: Maybe a -> b -> (a -> b) -> b
    maybe' m d f = maybe d f m

    newAccount
        :: HashMap UserLogin AccountData
        -> PeerTraderAccount
        -> IO (HashMap UserLogin AccountData)
    newAccount m (PeerTraderAccount n _ (Just userKey) _ _) = do
        mUser <- flip runDbConn g $ get userKey
        -- Get the most recent account from the API or database
        maybe' mUser (return m) $ \user -> do
            pAcct <- initProsperAccount user
            picksAccount <- flip runDbConn g $ getP2PPicksAccount n
            acct <- accountData user pAcct picksAccount
            return $ H.insert n acct m
      where
        -- Attempt to get Prosper account from Prosper API.
        -- if fail, log and return result from db
        initProsperAccount ui =
            P.account ui
          `catches`
            [ Handler unauthorizedAccount
            , Handler failProsperAcct ]
        unauthorizedAccount (UnauthorizedException msg) = do
            debugM "Initialization" $
                "Stale account for user " ++ show n ++ ": " ++ show msg
            flip runDbConn g $ latestProsperAccount n
        failProsperAcct e = do
            debugM "Initialization" $
                "Could not retrieve Prosper account for user " ++
                show n ++ ": " ++ show (e :: SomeException)
            flip runDbConn g $ latestProsperAccount n
    newAccount m _ = return m

-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes =
    [ ("/login", with auth handleLoginSubmit)
    , ("/logout", with auth handleLogout)
    , ("/changepassword", with auth handleChangePassword)
    , ("/sendForgotEmail", sendForgotPasswordEmail)
    , ("/resetPassword", handleResetPassword)
    , ("/forgotPassword", render "forgot")
    , ("/new_user", handleNewUser)
    , ("/verifynewuser", handleVerify)
    , ("/checkterms", requireLogIn handleCheckTerms)

    -- Data
    , ("/streamdata", requireLogIn handleStreamData) -- TODO add channel route
    , ("/prosperaccount", requireLogIn accountHandler)
    , ("/prosperaccountlatest", requireLogIn accountDataHandler)
    , ("/statistics", requireLogIn statisticsHandler)
    , ("/prosperaccounttimeseries", requireLogIn accountTimeSeriesHandler)
    , ("/activity", requireLogIn activity)
    , ("/p2ppicksaccount", requireLogIn p2paccountHandler)

    -- Strategies
    , ("/autofilter", requireLogIn autoFilterHandler)
    , ("/p2ppicks", requireLogIn p2ppicksHandler)

    -- Control
    , ("/prosperactivate", requireLogIn prosperAccountController)
    , ("/strategymanagement", requireLogIn strategyManagement)

    -- Admin
    , ("/peertraderusers", adminJSON peertraderUsers)
    , ("/adminstatistics", adminJSON mostRecentStats)
    , ("/updatestats", withAdminUser handleUpdateStats)

    , ("", serveDirectory "static")
    ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "PeerTrader" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 604800) -- One week
    k <- nestSnaplet "awsKeys" awsKeys initAWSKeys
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    ek <- nestSnaplet "ekg" ekg ekgInit
    p2p <- nestSnaplet "p2ppicks" p2ppicks p2ppicksInit

    addRoutes routes
    addAuthSplices h auth

    -- Add extra splices
    modifyHeistState $ bindSplice "ifAdmin" ifAdmin

    config <- getSnapletUserConfig
    dbName <- configLookup defaultDBName config "groundhog.name"
    dbUser <- configLookup defaultDBUser config "groundhog.user"
    dbHost <- configLookup defaultDBHost config "groundhog.host"

    let connString = "dbname=" ++ dbName ++ " user=" ++ dbUser ++ " host=" ++ dbHost
    g <- createPostgresqlPool connString 3
    liftIO $ withPostgresqlConn connString $ runDbConn $ runMigration $ do
        -- Migrations go here
        migrate (undefined :: AutoFilter)
        migrate (undefined :: Strategy AutoFilter)
        migrate (undefined :: Strategy P2PPicksType)
        migrate (undefined :: InvestState)
        migrate (undefined :: StrategyState AutoFilter)
        migrate (undefined :: StrategyState P2PPicksType)
        migrate (undefined :: Investment)
        migrate (undefined :: Statistics)
        migrate (undefined :: ProsperAccount)
        migrate (undefined :: ListingResult)
        migrate (undefined :: ActiveStrategy AutoFilter)
        migrate (undefined :: ActiveStrategy P2PPicksType)
        migrate (undefined :: P2PPicksAccount)
        migrate (undefined :: User)
        migrate (undefined :: PeerTraderAccount)
        migrate (undefined :: P2PPicksResult)
        migrate (undefined :: NewUser)
    ps <- initializePeerTrader

    accts <- liftIO $ initializeDatabase g
    commandChan <- liftIO $ atomically newTChan
    liftIO $ startClient commandChan accts

    let ptApp = App h s a d ek k p2p g ps accts commandChan
    _ <- liftIO . forkIO $ scheduleLoop ptApp
    return ptApp
  where
    configLookup d c n = liftIO (C.lookupDefault d c n)
    defaultDBHost = "localhost"
    defaultDBName = "peertrader"
    defaultDBUser = "peertrader"
