{-# LANGUAGE OverloadedStrings #-}
{-|

Module contaions high-level functions for working with Hazelcast.

-}


module Database.Hazelcast.Rest.Map
  ( Path
  , mkMapPath
  , insert
  , lookup
  , lookupMany
  , delete
  , getRq
  , postRq
  , deleteRq
  ) where

import qualified Data.ByteString as BS
import           Data.ByteString.Base64.URL
import           Database.Hazelcast.Rest.Core as Core
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import           Prelude hiding (lookup)

-- | Type represents url-safe (only valid url characters) path.
newtype Path = Path { unPath :: BS.ByteString }
  deriving (Show)

-- | Construct url-safe path. There is no map name encoding because it
-- may be set in hazelcast config file and after encoding nonexistent map name
-- can be obtained.
mkMapPath
  :: BS.ByteString -- ^ map name should not contain invalid url characters
  -> BS.ByteString
  -> Path
mkMapPath mapName key = Path $ BS.concat
  [ "/hazelcast/rest/maps/"
  , mapName
  , "/"
  , encode key]

-- | Insert value by path to Hazelcast.
insert :: MonadHazelcast m => Path -> BS.ByteString -> m Reply
insert (Path path) body = Core.sendRequest (postRq path body)

-- | Find value by path in Hazelcast.
lookup :: MonadHazelcast m => Path -> m Reply
lookup (Path path) = Core.sendRequest (getRq path)

-- | Find values by paths in Hazelcast.
lookupMany
  :: (MonadHazelcast m, Traversable t)
  => t Path
  -> m (t Reply)
lookupMany paths = Core.sendRequests qs
  where
    qs = fmap (getRq . unPath) paths

-- | Delete value by path in Hazelcast.
delete :: MonadHazelcast m => Path -> m Reply
delete (Path path) = Core.sendRequest (deleteRq path)

-- | Form GET request to Hazelcast.
getRq :: BS.ByteString -> HTTP.Request
getRq path = HTTP.defaultRequest
  { HTTP.method         = HTTP.methodGet
  , HTTP.path           = path
  , HTTP.requestVersion = HTTP.http11
  , HTTP.requestHeaders = [(HTTP.hAccept, "*/*")]
  }

-- | Form POST request to Hazelcast.
postRq :: BS.ByteString -> BS.ByteString -> HTTP.Request
postRq path body = HTTP.defaultRequest
  { HTTP.method         = HTTP.methodPost
  , HTTP.path           = path
  , HTTP.requestVersion = HTTP.http11
  , HTTP.requestHeaders =
    [ (HTTP.hAccept, "*/*")
    , (HTTP.hContentType, "text/plain") ]
  , HTTP.requestBody    = HTTP.RequestBodyBS body
  }

-- | Form DELETE request to Hazelcast.
deleteRq :: BS.ByteString -> HTTP.Request
deleteRq path = HTTP.defaultRequest
  { HTTP.method         = HTTP.methodDelete
  , HTTP.path           = path
  , HTTP.requestVersion = HTTP.http11
  , HTTP.requestHeaders = [ (HTTP.hAccept, "*/*") ]
  }
