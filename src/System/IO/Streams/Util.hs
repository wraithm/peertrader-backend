module System.IO.Streams.Util where

import           Control.Monad     (when)

import           Data.Maybe        (isJust)

import           System.IO.Streams as Streams

bifurcate :: InputStream a -> OutputStream a -> OutputStream a -> IO ()
bifurcate is os1 os2 = do
    x <- Streams.read is
    Streams.write x os1
    Streams.write x os2
    when (isJust x) $ bifurcate is os1 os2
