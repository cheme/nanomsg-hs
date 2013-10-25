{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Monad(forever)
import qualified Data.ByteString.Char8 as BS
import System.NanoMsg.C.NanoMsg
main :: IO ()
main = do
   args <- getArgs
   -- testing alloc msg
   
   -- simple alloc
   alres <- nnAllocmsg 100000000 0
   case alres of
      Left a -> showEr a
      Right mem -> do
                      r <- nnFreemsg mem
                      case r of 
                         Just r' -> showEr r'
                         Nothing -> return ()
   -- try to overflow 
   alres <- nnAllocmsg 100000000000 0
   case alres of
      Left a -> showEr a
      Right mem -> do
                      r <- nnFreemsg mem
                      case r of 
                         Just r' -> showEr r'
                         Nothing -> return ()
 
   -- auto alloc -- TODo valgrind it
   alres <- nnAllocmsg' 100000000 0
   case args of
     [host, port] -> do
       return ()
     _ -> do print " required args are anything , host , port"
  where
  showEr i = do
                putStrLn $ "Error " ++ show i
                label <- nnStrerror i
                putStrLn $ "Error " ++ show i ++ " = " ++ label

