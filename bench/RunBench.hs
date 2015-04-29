module Main where

import           Criterion.Main

import           Control.Exception          as X
import           Control.Exception.Enclosed
import           Control.Monad.Reader

import           Prosper
import           Prosper.Monad

someException :: SomeException -> IO ()
someException _ = putStrLn "Caught some exception..."

someException' :: SomeException -> Prosper ()
someException' _ = liftIO $ putStrLn "Caught some exception..."

noExceptions :: ProsperState -> IO ()
noExceptions = runReaderT updateAllListings

oldExceptions :: ProsperState -> IO ()
oldExceptions ps = runReaderT updateAllListings ps `X.catch` someException

enclosedExceptionsInside :: ProsperState -> IO ()
enclosedExceptionsInside = runReaderT (updateAllListings `catchAny` someException')

enclosedExceptionsOutside :: ProsperState -> IO ()
enclosedExceptionsOutside ps = runReaderT updateAllListings ps `catchAny` someException

foreverExceptions :: ProsperState -> IO ()
foreverExceptions ps = replicateM_ 10 $ enclosedExceptionsOutside ps

-- | This uses recursion to handle an exception, it starts a new loop.
exceptionLoop :: Int -> ProsperState -> IO ()
exceptionLoop 0 _ = return ()
exceptionLoop n ps = do
    runReaderT updateAllListings ps `catchAny` handleExceptionLoop
    exceptionLoop (n - 1) ps
  where
    handleExceptionLoop :: SomeException -> IO ()
    handleExceptionLoop _ = do
        putStrLn "Caught some exception..."
        exceptionLoop (n - 1) ps

main :: IO ()
main = do
    ps <- initializeProsper "prosper.cfg"
    defaultMain
        [ bgroup "update"
            [ bench "Update" (replicateM_ 10 $ noExceptions ps)
            ]
        , bgroup "exceptions"
            [ bench "No exception handling" (noExceptions ps)
            , bench "Old exception handling" (oldExceptions ps)
            , bench "Enclosed exceptions: forever inside" (enclosedExceptionsInside ps)
            , bench "Enclosed exceptions: forever outside" (enclosedExceptionsOutside ps)
            ]
        , bgroup "loop"
            [ bench "Forever" (foreverExceptions ps)
            , bench "Exception loop" (exceptionLoop 10 ps)
            ]
        ]
