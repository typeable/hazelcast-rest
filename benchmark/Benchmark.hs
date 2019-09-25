{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.List.NonEmpty as NE
import Data.Time
import Database.Hazelcast.Rest as Hazelcast
import Text.Printf

nRequests, nClients :: Int
nRequests = 100000
nClients  = 50


main :: IO ()
main = do
  ----------------------------------------------------------------------
  -- Preparation
  --
  let pingKey = mkMapPath "mapName" "PING"

  let
    connInfo = Hazelcast.defaultConnectInfo
      { connectHosts = NE.fromList
        [ HazelcastHost "localhost" 5701 ]
      }
  conn <- connect connInfo
  runHazelcast conn $ do
    _ <- Hazelcast.insert pingKey "PONG"
    return ()

  ----------------------------------------------------------------------
  -- Spawn clients
  --
  start <- newEmptyMVar
  done  <- newEmptyMVar
  replicateM_ nClients $ forkIO $ do
    runHazelcast conn $ forever $ do
      action <- liftIO $ takeMVar start
      action
      liftIO $ putMVar done ()

  let
    timeAction name nActions action = do
      startT <- getCurrentTime
      -- each clients runs ACTION nRepetitions times
      let nRepetitions = nRequests `div` nClients `div` nActions
      replicateM_ nClients $ putMVar start (replicateM_ nRepetitions action)
      replicateM_ nClients $ takeMVar done
      stopT <- getCurrentTime
      let
        deltaT     = realToFrac $ diffUTCTime stopT startT
        -- the real # of reqs send. We might have lost some due to 'div'.
        actualReqs = nRepetitions * nActions * nClients
        rqsPerSec  = fromIntegral actualReqs / deltaT :: Double
      putStrLn $ printf "%-15s %4dx%4dx%2d %10.2f Req/s"
        (name :: String) nActions nRepetitions nClients rqsPerSec

  ----------------------------------------------------------------------
  -- Benchmarks
  --
  timeAction "get" 1 $ do
    r <- Hazelcast.lookup pingKey
    unless (r == ReplyOk "PONG") $ error $ "bad hazelcast reply: " ++ show r
    return ()

  timeAction "get 1" 1 $ do
    _ <- Hazelcast.lookupMany (replicate 1 pingKey)
    return ()

  timeAction "get 50" 50 $ do
    _ <- Hazelcast.lookupMany (replicate 50 pingKey)
    return ()

  timeAction "get 100" 100 $ do
    _ <- Hazelcast.lookupMany (replicate 100 pingKey)
    return ()

  timeAction "get 1000" 1000 $ do
    _ <- Hazelcast.lookupMany (replicate 1000 pingKey)
    return ()

  timeAction "put" 1 $ do
    _ <- Hazelcast.insert pingKey "DONG"
    return ()
