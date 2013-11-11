{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |
-- Clone client Model One
-- 
-- Translated to Haskell by

module Main
  where

import Kvsimple
import qualified System.NanoMsg as N
import System.NanoMsg (Socket(..))

import Data.Binary
import Data.Hashable(Hashable)

import qualified Data.HashMap.Lazy as H
import Control.Concurrent (threadDelay)
import Control.Exception(AsyncException,catch)

nothing :: Maybe Integer
nothing = Nothing


main :: IO()
main = do
  -- Prepare our context and publisher socket
    updates <- N.socket N.Sub
--    N.setLinger updates 100
    N.setSubSubscribe updates ""

--    N.connect updates "tcp://127.0.0.1:5556"
    _ <- N.connect updates "tcp://127.0.0.1:5556"

    threadDelay 2000
    let kvmap = H.empty
    let sequence = 0
    recvKV sequence updates kvmap
    threadDelay 2000000
  where
    recvKV seq upd kvm = do
      kvmsg :: (KVMsg Integer Integer) <- recv upd
      let map = store kvmsg kvm
      let s = seq + 1
      recvKV s upd kvm
     `catchAsync` \_ ->  putStrLn (" Interrupted\n" ++ show (seq ) ++ " messages in")

catchAsync ::  IO a -> (AsyncException -> IO a) -> IO a
catchAsync = catch
