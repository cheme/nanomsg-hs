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

nothing :: Maybe Integer
nothing = Nothing


main :: IO()
main = do
  -- Prepare our context and publisher socket
    updates <- N.socket N.Sub
--    N.setLinger updates 100

    N.bind updates "tcp://127.0.0.1:5556"
    N.receive updates []
    return ()
