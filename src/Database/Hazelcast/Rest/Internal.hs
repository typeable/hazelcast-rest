{-|

Module contains types and functions required for connection with Hazelcast.

-}

module Database.Hazelcast.Rest.Internal where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.List.NonEmpty as NE
import           Data.Word
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP

-- | Type represents Hazelcast host.
data HazelcastHost = HazelcastHost
  { hostAddr :: BS.ByteString
  , hostPort :: Word16
  }

-- | Type represents connection information.
data ConnectInfo = ConnectInfo
  { connectHosts          :: NE.NonEmpty HazelcastHost
  , connectMaxConnections :: Int
  , connectTimeoutSeconds :: Int
  }

-- | Type provides possible Hazelcast responses.
data Reply
  = ReplyOk BS.L.ByteString
  | ReplyEmpty
  | ReplyError Int
  deriving (Eq, Show)

-- | Send HTTP request to Hazelcast.
sendHttpRequest
  :: HTTP.Manager
  -> HTTP.Request
  -> IO Reply
sendHttpRequest m req =
  readReply <$> HTTP.httpLbs req m

-- | Read status code from Hazelcast response and depending on status code
-- returns one of Reply constructor.
readReply :: HTTP.Response BS.L.ByteString -> Reply
readReply r = case HTTP.statusCode (HTTP.responseStatus r) of
  204 -> ReplyEmpty
  200 -> ReplyOk $ HTTP.responseBody r
  x   -> ReplyError x
