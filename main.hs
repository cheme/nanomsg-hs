{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Foreign
import Foreign.C.Types
import Control.Monad(forever)
import qualified Data.ByteString.Char8 as BS
import System.NanoMsg.C.NanoMsg
import System.NanoMsg.C.NanoMsgStruct
import Control.Concurrent(forkIO)
import Control.Concurrent.MVar(newEmptyMVar)
import Control.Concurrent.MVar(putMVar)
import Control.Concurrent.MVar(takeMVar)
import Control.Concurrent(forkOS)
import Foreign.C.String(peekCStringLen)
import Foreign.C.String(newCString)
import Foreign.C.String(newCAStringLen)
import Foreign.C.String(peekCString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal(ByteString(PS))
import Data.List(foldl')
import Control.Monad(foldM_)
import qualified System.NanoMsg as N
main :: IO ()
main = do

 args <- getArgs
   
   -- parameter handling
 case args of 
  [host, port] -> do
   sockpair1 <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
   nnBind sockpair1 "inproc://tsest"
--   nnConnect sockpair1 "tcp://127.0.0.1:5556"
 
   let m = "Hello wordk" :: String
   let l = length m
   p <- nnAllocmsg l 0 -- TODO link on bytestring to do unsafe zero copy -- here does not make sense
   p <- getEr p
   foldM_  (\a c -> pokeByteOff p a c >> return (a + 1)) 0 m
   m' <- peekCString (castPtr p)
--   r  <- nnSendDyn sockpair2 (castPtr p) [] -- TODO bracket it ??
 --  si <- getEr r
 --  case r of Left _ -> nnFreemsg p
 --            Right _ -> return Nothing
 
   putStrLn "12"
--   (sr, v) <- nnRecvDyn' sockpair1 []
--   si1 <- getEr sr
--   r1 <- peekCStringLen ((castPtr v),  si1)
--   putStrLn r1
   putStrLn "tsrtr"
   sockpair2 <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
   let m2 = "Hello word2k" :: String
   let l2 = length m2
   p2 <- nnAllocmsg (l2) 0 -- TODO link on bytestring to do unsafe zero copy -- here does not make sense
   p2 <- getEr p2
   foldM_  (\a c -> pokeByteOff p2 a c >> return (a + 1)) 0 m2
   r <- nnSendDyn sockpair2 (castPtr p2) []
   si <- getEr r
   case r of Left _ -> nnFreemsg p2
             Right _ -> return Nothing
   (sr, v) <- nnRecvDyn sockpair1 []
   si2 <- getEr sr
   r2 <- withForeignPtr (castForeignPtr v) (\p' ->  peekCStringLen (p',  si2))
 

   updates <- nnSocket AF_SP NN_PAIR >>= getEr
   nnBind updates "inproc://test12"
   (sr , v ) <- nnRecvDyn updates []
   putStrLn $ show sr
   putStrLn "* Test nn_socket"
   xsocksurveyor <- nnSocket AF_SP_RAW NN_SURVEYOR >>= getEr -- ok
   xsockrespondent <- nnSocket AF_SP_RAW NN_RESPONDENT >>= getEr -- ok
   xsockbus <- nnSocket AF_SP_RAW NN_BUS >>= getEr -- ok
   xsockpair <- nnSocket AF_SP_RAW NN_PAIR >>= getEr -- ok
   xsockpush <- nnSocket AF_SP_RAW NN_PUSH >>= getEr -- ok
   xsockpull <- nnSocket AF_SP_RAW NN_PULL >>= getEr -- ok
   xsockreq <- nnSocket AF_SP_RAW NN_REQ >>= getEr -- ok
   xsockrep <- nnSocket AF_SP_RAW NN_REP >>= getEr -- ok
   -- TODO QuickCheck this (combinatory code)
   socksurveyor <- nnSocket AF_SP NN_SURVEYOR >>= getEr -- ok
   sockrespondent <- nnSocket AF_SP NN_RESPONDENT >>= getEr -- ok
   sockbus <- nnSocket AF_SP NN_BUS >>= getEr -- ok
   sockpair1 <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
   sockpair2 <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
   sockpair1' <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
   sockpair2' <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
   sockpush <- nnSocket AF_SP NN_PUSH >>= getEr -- ok
   sockpull <- nnSocket AF_SP NN_PULL >>= getEr -- ok
   sockreq <- nnSocket AF_SP NN_REQ >>= getEr -- ok
   sockrep <- nnSocket AF_SP NN_REP >>= getEr -- ok
   nnBind sockpair1 "inproc://test1"
   nnConnect sockpair2 "inproc://test1"
 
 -- testing emfile error -> ok (EMFILE) "but disabled here"
--   forever $ do 
--                   eSockpub' <- nnSocket AF_SP_RAW NN_PAIR
--                   sockpub' <- getEr eSockpub' -- supported
--                   return ()
   putStrLn "* Test nn_cmsg"
   -- message from send msg first example as non foreign
   (v1, l1) <- newCAStringLen "Hello"
   (v2, l2) <- newCAStringLen "World"
   let iovec1 = NNIoVec (castPtr v1) (fromIntegral l1)
   let iovec2 = NNIoVec (castPtr v2) (fromIntegral l2)
   let nbvec = 2
   iovec <- mallocArray nbvec
   pokeArray iovec [iovec1,iovec2]
--   cmsghdr <- newNnCmsghdr
   let cmsghdr = NNCMsgHdr 3 1 1 -- level 1 type 1 size 0 only, dynamic length only
   nncdata <- ptrToCData [(cmsghdr, "123")] -- TODO implement api which create hdr
   
   ptcdata <- malloc :: IO (Ptr CInt) -- any ptr TODO in api alloca or foreignfree
   poke (castPtr ptcdata) (castPtr nncdata)
   let msghdr = NNMsgHdr (castPtr iovec) (fromIntegral nbvec) (castPtr ptcdata) 1 -- Not implemented yet in NNMsg
   let msghdr = NNMsgHdr (castPtr iovec) (fromIntegral nbvec) nullPtr 0 -- Not implemented yet in NNMsg
   cmsg <- cmsgFirstHdr msghdr
   case cmsg of
     Nothing -> putStrLn "ok"
     Just cm -> cmsgData cm >>= (putStrLn . show)

--   freeNnIovec iovec1
--   freeNnIovec iovec2
--   freeNnCmsghdr cmsghdr
--   freeNnMsghdr msghdr
   -- message from send msg second example as foreign, with alloc
   
  
   putStrLn "* Test sendmsg recvmsg"
   si <- nnSendmsg sockpair2 msghdr [] >>= getEr

   (v1R, l1R) <- newCAStringLen "aaaaaaa" --Size is too long = wrong get content out of buffer
   (v2R, l2R) <- newCAStringLen "hhh" -- Size is too short = ok some content is discard
   let iovec1R = NNIoVec (castPtr v1R) (fromIntegral l1R)
   let iovec2R = NNIoVec (castPtr v2R) (fromIntegral l2R)
   iovecR <- mallocArray nbvec
   pokeArray iovecR [iovec1R,iovec2R]
   let msghdrRec = NNMsgHdr (castPtr iovecR) (fromIntegral nbvec) nullPtr 0
   -- overwright in existing struct
   si <- nnRecvmsg sockpair1 msghdrRec [] >>= getEr
   putStrLn $ show si
   peekCString v1R >>= putStrLn
   peekCString v2R >>= putStrLn

   -- Dynamic MSG alloc 
   let m3 = "Hello word3k." :: String
   let l3 = length m3
   putStrLn $ show l3
   p3' <- nnAllocmsg (l3) 0 -- TODO link on bytestring to do unsafe zero copy -- here does not make sense
   p3 <- getEr p3'
   ptS <- malloc :: IO (Ptr (Ptr CChar)) -- any ptr TODO in api alloca or foreignfree
   poke (castPtr ptS) p3
   foldM_  (\a c -> pokeByteOff (castPtr p3) a c >> return (a + 1)) 0 m3
   let iovec3 = NNIoVec (castPtr ptS) (fromIntegral nN_MSG)
   iovecA3 <- mallocArray 1
   pokeArray iovecA3 [iovec3]
   let msghdr3 = NNMsgHdr (castPtr iovecA3) 1 nullPtr 0

   si <- nnSendmsg sockpair2 msghdr3 [] >>= getEr
   putStrLn $ show si
    --TODO
   ptR <- malloc :: IO (Ptr (Ptr CChar)) -- any ptr TODO in api alloca or foreignfree
   let iovec3R = NNIoVec (castPtr ptR) (fromIntegral nN_MSG)
   iovec3AR <- mallocArray 1 -- only one element everytime -> use simple ptr alloca
   pokeArray iovec3AR [iovec3R]
   let msghdrRec3 = NNMsgHdr (castPtr iovec3AR) (fromIntegral 1) nullPtr 0
   si <- nnRecvmsg sockpair1 msghdrRec3 [] >>= getEr
   putStrLn $ show si
   p <- peek ptR 
   peekCStringLen (p, (fromIntegral si))  >>= putStrLn
   nnFreemsg (castPtr p) -- TODO bracket??
   -- on api do not new but allocaArray or foreignfree (given fix msg length)
   free ptS
   free ptR
   free ptcdata
   free v1
   free v2
   free iovec
   free v1R 
   free v2R
   free iovecR

   putStrLn "* Test nn_device"
{-
   m1 <- newEmptyMVar
   pid1 <- forkIO $ forever $ do
     putStrLn "Start1"
     -- recv pair then send tcp
     v <- alloca (\p -> nnRecv' sockpair1 (castPtr p) (toInteger cintsize) [] >>= getEr >> peekInteger (p :: Ptr CInt)) -- TODO peek with returned size!!
     putStrLn $ "Fork 1 : " ++ show v
     threadDelay 200000
     if v > 5 then putMVar m1 () else do
       si <- alloca (\p -> poke p (fromInteger v :: CInt) >> nnSend' sockpair2' (castPtr p) (toInteger cintsize) [] >>= getEr)
       if si == (toInteger cintsize) then return () else putStrLn "Size pb in send"

   m2 <- newEmptyMVar
   pid2 <- forkIO $ forever $ do
     putStrLn "Start2"
     p <- mallocForeignPtr :: IO (ForeignPtr CInt)
     nnRecv sockpair1' (castForeignPtr p) (toInteger cintsize) [] >>= getEr
     putStrLn "Start3"
     v <- withForeignPtr p peekInteger
     putStrLn $ "Fork 2 : " ++ show v
     threadDelay 200000
     pv <- mallocForeignPtr :: IO (ForeignPtr CInt)
     withForeignPtr pv (\pv' -> poke pv' (fromInteger (v+1)))
     if v > 5 then putMVar m2 () else do
       si <- nnSend sockpair2 (castForeignPtr pv) (toInteger cintsize) [] >>= getEr
       if si == (toInteger cintsize) then return () else putStrLn "Size pb in send"

   threadDelay 200000
   si <- alloca (\p -> poke p (3 :: CInt) >> nnSend' sockpair2 (castPtr p) (toInteger cintsize) [] >>= getEr)
   if si == (toInteger cintsize) then return () else putStrLn "Size pb in send"



   takeMVar m2
   takeMVar m1
-}

   -- testing alloc msg
   putStrLn "* Test alloc/free"
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
   -- testing nn_term
   putStrLn "* Test nn_term"
   nnTerm
   eSockpub' <- nnSocket AF_SP_RAW NN_SURVEYOR
   sockpub' <- getEr eSockpub' -- ETERM


 

   return ()

  _ -> do print " required args are anything , host , port"
  where
  showEr i = do
                label <- nnStrerror i
                putStrLn $ "Error " ++ show i ++ " = " ++ label
  getEr (Left a) = showEr a >> return undefined
  getEr (Right b) = return b
  getErr (Just a) = showEr a
  getErr Nothing = return ()

ptrToCData :: [(NNCMsgHdr, BS.ByteString)] -> IO (Ptr ()) -- TODO transform to with fn or foreignfree returned
ptrToCData  a = do
  let totle = foldl' (\acc (h, bs)-> acc + BS.length bs + sizeOf h) 0 a -- TODO do not use BS.length
  ptr <- mallocBytes (totle)

  foldM_ (pokeEl (ptr)) 0 a  >> return (ptr)
    where pokeEl :: Ptr () -> Int -> (NNCMsgHdr, BS.ByteString) -> IO Int
          pokeEl ptr offset (hdr, bs@(PS pbs pof ple)) = do
                                        pokeElemOff (castPtr ptr) offset hdr
                                        --pokeElemOff ptr (offset + sizeOf hdr) bs
                                        withForeignPtr pbs $ \pbs' -> copyBytes ((castPtr ptr) `plusPtr` (offset + sizeOf hdr)) ((castPtr pbs') `plusPtr` pof) (ple - pof) -- TODO use unsafe bytestring api instead
                                        return (offset + sizeOf hdr + (ple - pof))

