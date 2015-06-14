{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Application where

import           Control.Applicative           ((<$>))
import           Control.Concurrent            (forkIO, killThread, threadDelay)
import           Control.Concurrent.STM
import           Control.Exception             (SomeException)
import           Control.Lens
import           Control.Monad.CatchIO         (catches)
import qualified Control.Monad.CatchIO         as E
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader          (ReaderT, ask, forever, local,
                                                void)
import           Control.Monad.State           (get)

import           Data.HashMap.Strict           as H
import           Data.Maybe                    (isJust)
import           Data.Monoid                   (First)
import           Data.Pool
import           Data.Traversable              as T

import           Database.Groundhog.Core       hiding (get)
import           Database.Groundhog.Postgresql hiding (get)

import           Snap.Extras.CoreUtils         (finishEarly)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Ekg              (Ekg)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.P2PPicks
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.SES
import           Snap.Snaplet.Session

import           Logging                       (debugM)
import           Prosper                       (Account,
                                                UnauthorizedException (..),
                                                User (..), account)
import           Prosper.Monad                 (Prosper, ProsperState, forkP,
                                                runProsper)

import           PeerTrader.Account.Web
import           PeerTrader.Socket.Command
import           PeerTrader.Types

type Accounts = TVar (HashMap UserLogin AccountData)

data App = App
    { _heist        :: Snaplet (Heist App)
    , _sess         :: Snaplet SessionManager
    , _auth         :: Snaplet (AuthManager App)
    , _db           :: Snaplet Postgres
    , _ekg          :: Snaplet Ekg
    , _awsKeys      :: Snaplet AWSKeys
    , _p2ppicks     :: Snaplet P2PPicksKeys

    , _gh           :: Pool Postgresql
    , _prosperState :: ProsperState
    , _accounts     :: Accounts
    , _opsWriteChan :: TChan Command
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
    getPostgresState = with db get
    setLocalPostgresState s = local (set (db . snapletValue) s)

instance ConnectionManager App Postgresql where
    withConn f app = withConn f (view gh app)
    withConnNoTransaction f app = withConnNoTransaction f (view gh app)

runGH :: ConnectionManager b conn
    => DbPersist conn (NoLoggingT IO) a
    -> Handler b v a
runGH f = withTop' id $ do
    cm <- ask
    liftIO $ runNoLoggingT (withConn (runDbPersist f) cm)

type AppHandler = Handler App App
type AuthAppHandler = Handler App (AuthManager App)
type AppReader = ReaderT App (NoLoggingT IO)

handleProsper :: Prosper a -> AppHandler a
handleProsper p = do
    ps <- use prosperState
    liftIO $ runProsper ps p

-- Code for User-specific functions -------------------------------------------

withCurrentUser :: (UserLogin -> AppHandler a) -> AppHandler a
withCurrentUser f = do
    n <- fmap userLogin <$> with auth currentUser
    maybe badUser f n
  where
    badUser = finishEarly 401 "Not logged in!"

withCurrentUser_ :: (UserLogin -> AppHandler ()) -> AppHandler ()
withCurrentUser_ = void . withCurrentUser

getLogger :: UserLogin -> AppHandler (Maybe String)
getLogger n = do
    acct <- getAccount n
    return $ show . username . _prosperUser <$> acct

existsAccount :: UserLogin -> AppHandler Bool
existsAccount n = isJust <$> getAccount n

getAccount :: UserLogin -> AppHandler (Maybe AccountData)
getAccount n = do
    accts <- liftIO . readTVarIO =<< use accounts
    return $ accts ^. at n

putAccount :: UserLogin -> AccountData -> AppHandler ()
putAccount n acct = do
    t <- use accounts
    liftIO . atomically $ modifyTVar' t (H.insert n acct)

removeAccount :: UserLogin -> AppHandler ()
removeAccount n = do
    t <- use accounts
    liftIO . atomically $ modifyTVar' t (H.delete n)

getAccountVar :: UserLogin -> Getting (First a) AccountData a -> AppHandler (Maybe a)
getAccountVar n l = do
    acct <- getAccount n
    return $ acct ^? _Just.l

putAccountVar :: UserLogin -> Setter' AccountData a -> a -> AppHandler ()
putAccountVar n l x = void $ do
    acct <- getAccount n
    let acct' = set (_Just.l) x acct
    T.mapM (putAccount n) acct'

-- TODO Abstract out reading account lens stuff
updateUserVar :: UserLogin -> Getter AccountData (TVar a) -> a -> AppHandler ()
updateUserVar n l x = do
    t <- getAccountVar n l
    maybe (return ()) (\t' -> liftIO . atomically $ writeTVar t' x) t
    -- TODO Report error if this fails

readUserVar :: UserLogin -> Getter AccountData (TVar a) -> AppHandler (Maybe a)
readUserVar n l = do
    t <- getAccountVar n l
    T.mapM (liftIO . readTVarIO) t
    -- TODO Report error if this fails

initAccountUpdate :: UserLogin -> Int -> AppHandler ()
initAccountUpdate n delay = do
    acct <- getAccount n
    case acct of
        Just (AccountData
            { _prosperUser = i
            , _prosperAccount = a
            , _prosperAccountThread = Nothing
            }) -> do
            tid <- liftIO . forkIO . forever $
                updateAccountLoop i a
            putAccountVar n prosperAccountThread (Just tid)
        _ -> return ()
  where
    updateAccountLoop :: User -> TVar Account -> IO ()
    updateAccountLoop i a = do
        newAccount <- account i
        _ <- (atomically . writeTVar a) newAccount
        threadDelay delay
      `catches`
        [ E.Handler staleAccountException
        , E.Handler accountExceptionHandler ]

    -- TODO this is blatantly wrong. Need to disable account update.
    -- Need to write in the database that the account is stale.
    -- Need to reactivate when the user changes their account data.
    -- NEED BETTER EXCEPTION HANDLING IN THIS WHOLE THING.
    -- Async exceptions? Manager thread!? LOTS OF STUFF.
    staleAccountException :: UnauthorizedException -> IO ()
    staleAccountException (UnauthorizedException msg) =
        debugM (show n) $ "Account out of date: " ++ show msg

    accountExceptionHandler :: SomeException -> IO ()
    accountExceptionHandler e = debugM (show n) $
        "Caught some exception updating account... " ++ show e ++ " Continuing..."

killAccountUpdate :: UserLogin -> AppHandler ()
killAccountUpdate n = do
    acct <- getAccount n
    case acct of
        Just (AccountData { _prosperAccountThread = Just t }) -> do
            liftIO $ killThread t
            putAccountVar n prosperAccountThread Nothing
        _ -> return ()

getProsperAccount :: UserLogin -> AppHandler (Maybe Account)
getProsperAccount n = readUserVar n prosperAccount
