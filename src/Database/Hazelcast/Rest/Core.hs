{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-|

Module provides Hazelcast monad and functions for opening connection and
sending requests.

-}


module Database.Hazelcast.Rest.Core
  ( connect
  , HazelcastEnv(..)
  , Hazelcast(..)
  , MonadHazelcast(..)
  , runHazelcast
  , sendRequest
  , sendRequests
  , defaultConnectInfo
  -- reexport
  , Internal.Reply(..)
  , Internal.ConnectInfo(..)
  , Internal.HazelcastHost(..)
  ) where

import           Control.Monad.Reader
import           Data.IORef
import qualified Data.List.NonEmpty as NE
import           Database.Hazelcast.Rest.Cluster
import           Database.Hazelcast.Rest.Internal as Internal
import qualified Network.HTTP.Client as HTTP


-- | Open connection with Hazelcast using connection information.
connect :: ConnectInfo -> IO HazelcastEnv
connect ConnectInfo{..} = do
  let
    ss = HTTP.defaultManagerSettings
      { HTTP.managerConnCount       = connectMaxConnections
      , HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro
        $ connectTimeoutSeconds * 1000000
      }
  manager <- HTTP.newManager ss
  clusterInfo <- ClusterInfo <$> newIORef (NE.toList connectHosts)
  pure (HazelcastEnv manager clusterInfo)

-- | 'HazelcastEnv' provides opened connection and cluster information.
data HazelcastEnv = HazelcastEnv
  { henvConnection  :: HTTP.Manager
  , henvClusterInfo :: ClusterInfo
  }

-- | 'Hazelcast' type is a wrapper around ReaderT monad contains 'HazelcastEnv'
-- type to read.
newtype Hazelcast a = Hazelcast
  { unHazelcast :: ReaderT HazelcastEnv IO a
  } deriving (Monad, MonadIO, Functor, Applicative)

-- | 'MonadHazelcast' provides function to lift Hazelcast type to another.
class Monad m => MonadHazelcast m where
  liftHazelcast :: Hazelcast a -> m a

instance MonadHazelcast Hazelcast where
  liftHazelcast = id

-- | Hazelcast monad runner.
runHazelcast :: HazelcastEnv -> Hazelcast a -> IO a
runHazelcast hEnv (Hazelcast hazelcast) =
  runReaderT hazelcast hEnv

-- | Send HTTP request to Hazelcast using 'MonadHazelcast'.
sendRequest
  :: (MonadHazelcast m)
  => HTTP.Request
  -> m Reply
sendRequest r =
  liftHazelcast . Hazelcast $ do
    m <- asks henvConnection
    c <- asks henvClusterInfo
    HazelcastHost{..} <- liftIO $ roundRobinHost c
    let
      req = r
        { HTTP.host = hostAddr
        , HTTP.port = fromIntegral hostPort }
    liftIO $ Internal.sendHttpRequest m req

-- | Send many HTTP requests to Hazelcast using 'MonadHazelcast'.
sendRequests
  :: (MonadHazelcast m, Traversable t)
  => t HTTP.Request
  -> m (t Reply)
sendRequests rqs =
  liftHazelcast . Hazelcast $ do
    m <- asks henvConnection
    c <- asks henvClusterInfo
    HazelcastHost{..} <- liftIO $ roundRobinHost c
    let
      modifyReq r = r
        { HTTP.host = hostAddr
        , HTTP.port = fromIntegral hostPort }
    liftIO $ traverse (Internal.sendHttpRequest m . modifyReq) rqs

-- | Default Hazelcast connection information.
defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo
  { connectHosts          = NE.fromList [HazelcastHost "localhost" 5701]
  , connectMaxConnections = 50
  , connectTimeoutSeconds = 10
  }
