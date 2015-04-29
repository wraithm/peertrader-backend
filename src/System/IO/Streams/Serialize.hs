module System.IO.Streams.Serialize where

import           Data.ByteString
import           Data.Serialize
import           System.IO.Streams as Streams

decodeFromStream :: Serialize a => InputStream ByteString -> IO (InputStream a)
decodeFromStream = Streams.map (fromEither . decode)
  where
    fromEither (Left s) = error s
    fromEither (Right x) = x

encodeToStream :: Serialize a => InputStream a -> IO (InputStream ByteString)
encodeToStream = Streams.map encode
