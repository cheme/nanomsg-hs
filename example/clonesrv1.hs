-- |
-- Clone server Model One


module Main
  where

import Kvsimple
import Kvsimple(KVMsg(..))
import System.NanoMsg (Socket(..))
import qualified System.NanoMsg as N

import Control.Exception.Base(AsyncException(UserInterrupt))

import qualified Data.HashMap.Lazy as H
import Control.Concurrent (threadDelay)
import System.Random(randomRIO)
import Control.Exception(catch)


main :: IO()
main = do
  -- Prepare our context and publisher socket
    publisher <- N.socket N.Pub
--    N.bind publisher "tcp://*:5556"
    N.bind publisher "tcp://127.0.0.1:5556"
    threadDelay (200 * 1000) -- 200 ms

    let sequence = 0
    let kvmap = H.empty

   -- try:
    --    while True:
    sendKV sequence publisher kvmap
  where
    sendKV seq pub kvm = do
      -- Distribute as key-value message
      let s = seq + 1
      k <- randomRIO (1 :: Integer, 10000)
      b <- randomRIO (1 :: Integer, 1000000)
      let kvmsg = KVMsg (Just k) s (Just b)
      send kvmsg pub
      threadDelay (2 * 1000) -- 200 ms
      let map = store kvmsg kvm
      sendKV s pub map
     `catchAsync` \_ ->  putStrLn (" Interrupted\n" ++ show (seq +1) ++ " messages out")



catchAsync ::  IO a -> (AsyncException -> IO a) -> IO a
catchAsync = catch
