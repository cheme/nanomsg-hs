{-# LANGUAGE OverloadedStrings #-}
-- | Tests of C bindings.
-- Just a few unitary tests.
module C where
import System.NanoMsg.C.NanoMsg
import System.NanoMsg.C.NanoMsgStruct
import Test.HUnit
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
import Foreign
import Foreign.C.Types
import Control.Concurrent(threadDelay)



testNnSocket :: IO Test
testNnSocket = do
   -- testing nn_socket (c level : return a generic socket)
   eSockpub <- nnSocket AF_SP NN_PUB
   sockpub <- getEr eSockpub
   
   eSocksub <- nnSocket AF_SP NN_SUB
   socksub <- getEr eSockpub

   eSockpub2 <- nnSocket AF_SP_RAW NN_PUB
   eSocksub2 <- nnSocket AF_SP_RAW NN_SUB

   return $ "Socket creation" ~:
     [ isRight eSockpub ~? "Failed socket init"
     , isRight eSocksub ~? "Failed socket init"
     , Left EAFNOSUPPORT ~=? eSockpub2
     , Left EAFNOSUPPORT ~=? eSocksub2
     ]

testNnClose :: IO Test
testNnClose = do
   s' <- nnSocket AF_SP NN_PUB >>= getEr
   c1 <- nnClose s'
   c2 <- nnClose s'
   return $ "Socket close" ~:
     [ Nothing    ~=? c1
     , Just EBADF ~=? c2
     ]


testNnOptions :: IO Test
testNnOptions = do
  sockpub <- nnSocket AF_SP NN_PUB >>= getEr
 
  let cintsize = sizeOf (undefined :: CInt)
  (err, val, size) <- allocaBytes cintsize (\p -> nnGetsockopt sockpub NN_SOL_SOCKET NN_DOMAIN p cintsize)
  v1 <- case err of
     Nothing -> peek (castPtr val :: Ptr CInt) >>= (return . (cIntToEnum :: CInt -> AddressFamilies))
  (err, val, size) <- allocaBytes cintsize (\p -> nnGetsockopt sockpub NN_SOL_SOCKET NN_PROTOCOL p cintsize)
  v2 <- case err of
     Nothing -> peek (castPtr val :: Ptr CInt) >>= (return . (cIntToEnum :: CInt -> NnProtocol))
     Just e ->  undefined
  let linger = 446654
  let clinger = fromIntegral linger :: CInt
  alloca (\p -> poke p clinger >> nnSetsockopt sockpub NN_SOL_SOCKET NN_LINGER (castPtr p) (fromIntegral (sizeOf clinger))) >>= getErr 
   
  (err, val, size) <- allocaBytes cintsize (\p -> nnGetsockopt sockpub NN_SOL_SOCKET NN_LINGER p cintsize)
  v3 <- case err of
     Nothing -> peekInt (castPtr val :: Ptr CInt)
     Just e -> undefined
  return $ "Socket options" ~:
     [ AF_SP  ~=? v1
     , NN_PUB ~=? v2
     , linger ~=? v3
     ]

testNnBindConnect :: IO Test
testNnBindConnect = do
   sockpush <- nnSocket AF_SP NN_PUSH >>= getEr -- ok
   sockpair1 <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
   sockpair2 <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
   sockpair1' <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
   sockpair2' <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
  
   eppush <- nnBind sockpush "dummy://localhost:8080"
   eppush' <- nnBind sockpair1 "tcp://localhost/8080"
   eppair1 <- nnBind sockpair1' "inproc://test"
   eppair1' <- getEr eppair1
   eppair2 <- nnBind sockpair2 "inproc://test"
   nnShutdown sockpair1' eppair1'
   eppair3 <- nnBind sockpair2 "inproc://test"
   eppair2' <- nnConnect sockpair1 "inproc://test"
   nnClose sockpair1
   nnClose sockpair1'
   nnClose sockpair2
   nnClose sockpair2'
   return $ "Socket Bind/connect" ~:
     [ Left EPROTONOSUPPORT ~=? eppush
     , Left EINVAL ~=? eppush'
     , isRight eppair1 ~? "Socket Bind error"
     , Left EADDRINUSE ~=? eppair2
     , isRight eppair3 ~? "Socket shutdown error"
     , isRight eppair2' ~? "Socket connect error"
     ]

initPairs = do
   sockpair1 <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
   sockpair2 <- nnSocket AF_SP NN_PAIR >>= getEr -- ok
   nnBind sockpair1 "inproc://test1"
   nnConnect sockpair2 "inproc://test1"
   return (sockpair1,sockpair2)

cintsize = sizeOf (undefined :: CInt)

testNnSendReceiveStatic :: IO Test
testNnSendReceiveStatic = do 
   (sockpair1,sockpair2) <- initPairs
--   threadDelay 200000
   si <- alloca (\p -> poke p (3 :: CInt) >> nnSend' sockpair2 (castPtr p) cintsize [] >>= getEr)
   if si == cintsize then return () else putStrLn "Size pb in send"
   v1 <- alloca (\p -> nnRecv' sockpair1 (castPtr p) cintsize [] >>= getEr >> peekInt (p :: Ptr CInt)) -- TODO peek with returned size!!

   pv <- mallocForeignPtr :: IO (ForeignPtr CInt)
   withForeignPtr pv (\pv' -> poke pv' (fromIntegral 4))
   si <- nnSend sockpair2 (castForeignPtr pv) cintsize [] >>= getEr
   if si == cintsize then return () else putStrLn "Size pb in send"
   p <- mallocForeignPtr :: IO (ForeignPtr CInt)
   nnRecv sockpair1 (castForeignPtr p) cintsize [] >>= getEr
   v2 <- withForeignPtr p peekInt
   nnClose sockpair1
   nnClose sockpair2
   return $ "Send Receive static" ~:
     [ 3 ~=? v1
     , 4 ~=? v2
     ]

testNnSendReceiveDyn :: IO Test
testNnSendReceiveDyn = do 
   (sockpair1,sockpair2) <- initPairs
   let m = "Hello wordk" :: String
   let l = length m
   p <- nnAllocmsg l 0 -- TODO link on bytestring to do unsafe zero copy -- here does not make sense
   p <- getEr p
   foldM_  (\a c -> pokeByteOff p a c >> return (a + 1)) 0 m
   m' <- peekCString (castPtr p)
   r  <- nnSendDyn sockpair2 (castPtr p) [] -- TODO bracket it ??
   si <- getEr r
   case r of Left _ -> nnFreemsg p
             Right _ -> return Nothing

   (sr, v) <- nnRecvDyn' sockpair1 []
   si1 <- getEr sr
   r1 <- peekCStringLen ((castPtr v),  si1)
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
   nnClose sockpair1
   nnClose sockpair2
   return $ "Send Receive dynamic" ~:
     [ m ~=? m'
     , (length m,m) ~=? (si1,r1)
     , (length m2,m2) ~=? (si2,r2)
     ]

testNnCmsg :: IO Test
testNnCmsg = do
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
   let r = case cmsg of
                        Nothing -> True
                        Just _  -> False
   return $ "Cmsg macro" ~:
    [ r ~? "Fail read write in cmsg"
    ]


testNnSendReceiveMsg :: IO Test
testNnSendReceiveMsg = do 
   (sockpair1,sockpair2) <- initPairs
   (v1, l1) <- newCAStringLen "Hello"
   (v2, l2) <- newCAStringLen "World"
   let iovec1 = NNIoVec (castPtr v1) (fromIntegral l1)
   let iovec2 = NNIoVec (castPtr v2) (fromIntegral l2)
   let nbvec = 2
   iovec <- mallocArray nbvec
   pokeArray iovec [iovec1,iovec2]
   let msghdr = NNMsgHdr (castPtr iovec) (fromIntegral nbvec) nullPtr 0 -- Not implemented yet in NNMsg
   si <- nnSendmsg sockpair2 msghdr [] >>= getEr

   (v1R, l1R) <- newCAStringLen "aaaaaaa" --Size is too long = wrong get content out of buffer
   (v2R, l2R) <- newCAStringLen "hhh" -- Size is too short = ok some content is discard
   let iovec1R = NNIoVec (castPtr v1R) (fromIntegral l1R)
   let iovec2R = NNIoVec (castPtr v2R) (fromIntegral l2R)
   iovecR <- mallocArray nbvec
   pokeArray iovecR [iovec1R,iovec2R]
   let msghdrRec = NNMsgHdr (castPtr iovecR) (fromIntegral nbvec) nullPtr 0
   -- overwright in existing struct
   si1 <- nnRecvmsg sockpair1 msghdrRec [] >>= getEr
   r1 <- peekCString v1R
   r2 <- peekCStringLen (v2R, (fromIntegral l2R))

   -- Dynamic MSG alloc 
   let m3 = "Hello word3k." :: String
   let l3 = length m3
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
    --TODO
   ptR <- malloc :: IO (Ptr (Ptr CChar)) -- any ptr TODO in api alloca or foreignfree
   let iovec3R = NNIoVec (castPtr ptR) (fromIntegral nN_MSG)
   iovec3AR <- mallocArray 1 -- only one element everytime -> use simple ptr alloca
   pokeArray iovec3AR [iovec3R]
   let msghdrRec3 = NNMsgHdr (castPtr iovec3AR) (fromIntegral 1) nullPtr 0
   si <- nnRecvmsg sockpair1 msghdrRec3 [] >>= getEr
   p <- peek ptR 
   r3 <- peekCStringLen (p, (fromIntegral si))
   nnFreemsg (castPtr p) -- TODO bracket??
   -- on api do not new but allocaArray or foreignfree (given fix msg length)
   free ptS
   free ptR
--   free ptcdata
   free v1
   free v2
   free iovec
   free v1R 
   free v2R
   free iovecR

   nnClose sockpair1
   nnClose sockpair2
   return $ "Send Receive MessagMessage" ~:
     [ 10 ~=? si1
     , "HelloWo" ~=? r1
     , "rld" ~=? r2
     , m3 ~=? r3
     ]



isRight (Right _) = True
isRight (Left _) = False
isLeft (Left _) = True
isLeft (Right _) = False

getEr (Left a) = showEr a >> return undefined
getEr (Right b) = return b
getErr (Just a) = showEr a
getErr Nothing = return ()
showEr i = do
  label <- nnStrerror i
  putStrLn $ "Error " ++ show i ++ " = " ++ label
 

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

