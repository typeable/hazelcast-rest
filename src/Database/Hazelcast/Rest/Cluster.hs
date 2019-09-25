{-|

Module provides type representing Hazelcast cluster and function to select a
host using Round-robin algorithm.

-}

module Database.Hazelcast.Rest.Cluster
  ( ClusterInfo(..)
  , roundRobinHost
  ) where

import Data.IORef
import Database.Hazelcast.Rest.Internal

-- | 'ClusterInfo' represents Hazelcast cluster.
newtype ClusterInfo = ClusterInfo (IORef [HazelcastHost])

-- | Select host from cluster using Round-robin algorithm.
roundRobinHost :: ClusterInfo -> IO HazelcastHost
roundRobinHost (ClusterInfo ref) = atomicModifyIORef' ref rotateHosts

-- | Return current head host in list and rotate list of hosts.
rotateHosts
  :: [HazelcastHost]
  -> ([HazelcastHost], HazelcastHost)
rotateHosts hosts = (rotate hosts, head hosts)
  where
    rotate l = zipWith const (drop 1 $ cycle l) l
