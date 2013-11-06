{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : System.NanoMsg
-- Copyright : (c) 2013 Émeric Chevalier
-- License : MIT
-- Maintainer : Émeric Chevalier <emericchevalier.pro@gmail.com>
-- Stability : experimental
-- Portability : non-portable
--
-- nanomsg haskell binding.
-- For documentation please refer to nanomsg man pages authored by Martin Sustrik. For details please refer to <http://nanomsg.org/documentation.html>
-- API follows loosely the API from zeromq-haskell library. 
-- To use functionality closer to C nanomsg please use export for System.NanoMsg.C.NanoMsg. Notably for receiving and sending directly Ptr or ForeignPtr (especially for instances of Storable).

module System.NanoMsg (
  -- ** Type Definition
    Socket
  , EndPoint
  , Flag
  , SndRcvFlags(..)
  -- ** Type Classes
  , SocketType
  , Sender
  , Receiver
  , HasRawSocket

  -- ** Socket Types
  , Pair(..)
  , Pub(..)
  , Sub(..)
  , Req(..)
  , Rep(..)
  , Pull(..)
  , Push(..)
  , Surveyor(..)
  , Respondent(..)
  , Bus(..)

  -- * General Operations
  , withSocket
  , withXSocket
  , socket
  , xsocket
  , close
  , bind
  , connect
  , shutdown
  , send
  , send'
  , usend
  , receive
  , ureceive
  , sendMsg
  , sendFMsg
  , receiveSingleMsg
  , receiveSingleFMsg
  , receiveMultiMsg
  , receiveMultiFMsg
  , freeMsg
  , forceTerm
  , device
  -- * Socket general Options
  , getDomain
  , getProtocol
  , getLinger
  , setLinger
  , getSendBuffer
  , setSendBuffer
  , getReceiveBuffer
  , setReceiveBuffer
  , getSendTimeout
  , setSendTimeout
  , getReceiveTimeout
  , setReceiveTimeout
  , getReconnectInterval
  , setReconnectInterval
  , getReconnectIntervalMax
  , setReconnectIntervalMax
  , getSendPrio
  , setSendPrio
  , getIPV4ONLY
  , setIPV4ONLY
  -- * Socket specific options
  , getReqResendInterval
  , setReqResendInterval
  , getSubSubscribe
  , setSubSubscribe
  , getSubUnsubscribe
  , setSubUnsubscribe
  , getSurveyorDeadline
  , setSurveyorDeadline
  -- * Message manipulation operations
  , newMultimessage
  , newMultimessage'
  , newMultifmessage
  , newMultifmessage'
  , newSinglemessage
  , newSinglemessage'
  , newSinglefmessage
  , newSinglefmessage'
  , addMultimessage
  , addMultimessage'
  , addMultifmessage
  , addMultifmessage'
  ) where
import System.NanoMsg.C.NanoMsg
import System.NanoMsg.C.NanoMsgStruct
import Control.Exception(bracket)
import qualified Data.ByteString as BS
import Data.ByteString.Internal(ByteString(PS))
import Foreign.Marshal.Utils(copyBytes)
import Data.ByteString.Unsafe(unsafeUseAsCStringLen)
import Foreign(newForeignPtr,withForeignPtr)
import Foreign.ForeignPtr(ForeignPtr,castForeignPtr,mallocForeignPtr,mallocForeignPtrArray)
import Foreign.Ptr(Ptr,nullPtr,plusPtr,castPtr)
import Foreign.Marshal.Alloc(alloca,allocaBytes,finalizerFree,malloc,free)
import Foreign.Marshal.Array(mallocArray,reallocArray,pokeArray,peekArray)
import Foreign.C.Types(CInt)
import Foreign.Storable(Storable,sizeOf,peek,poke)
import Control.Monad((<=<),liftM,unless)
import Foreign.C.String(withCString,peekCStringLen)

-- | Type for all socket
newtype Socket a = Socket NnSocket

-- | Type for nanoError
type Error = NnError

-- | Type for flag
type Flag = SndRcvFlags

-- | Type for messages with only one part
-- SingleMessage are restricted to only one part
-- MultiMessage are not
-- FMessage got automatic memory management (using ForeignPointer)
-- Using FMessages is not always safer, and could be problematics (memory release depends upon garbage collector so it is not deterministic).
-- TODO this and socket type should be rewritten to use gadt
data SingleMessage = SingleMessage NNMsgHdr Int
data SingleFMessage = SingleFMessage NNFMsgHdr Int 
data MultiMessage = MultiMessage NNMsgHdr 
data MultiFMessage = MultiFMessage NNFMsgHdr 

-- | Type for Endpoint of sockets
data EndPoint a = EndPoint NnSocket NnEndPoint

-- | Socket type for Pair protocol. See official documentation nn_pair(7)
data Pair = Pair

-- | Socket type for Pub of PubSub protocol. See official documentation nn_pubsub(7)
data Pub = Pub

-- | Socket type for Sub of PubSub protocol. See official documentation nn_pubsub(7)
data Sub = Sub

-- | Socket type for Req of ReqRep protocol. See official documentation nn_reqrep(7)
data Req = Req

-- | Socket type for Rep of ReqRep protocol. See official documentation nn_reqrep(7)
data Rep = Rep

-- | Socket type for Push of Pipeline protocol. See official documentation nn_pipeline(7)
data Push = Push

-- | Socket type for Pull of Pipeline protocol. See official documentation nn_pipeline(7)
data Pull = Pull

-- | Socket type for Surveyor of Survey protocol. See official documentation nn_survey(7)
data Surveyor = Surveyor

-- | Socket type for Respondent of Survey protocol. See official documentation nn_survey(7)
data Respondent = Respondent

-- | Socket type for Bus protocol. See official documentation nn_bus(7)
data Bus = Bus

-- | Sockets which can 'send'.
class (SocketType a) => Sender a

-- | Sockets which can 'receive'.
class (SocketType a) => Receiver a

-- | Socket having a raw implementation
class (SocketType a) => HasRawSocket a

-- | Socket underlying type
class SocketType a where
  nnSocketType :: a -> NnProtocol

-- | Msg class
class Message a where
  getCPtr :: a -> NNMsgHdr
class FMessage a where
  getCFPtr :: a -> NNFMsgHdr
instance Message SingleMessage where
  getCPtr (SingleMessage msg _) = msg
instance Message MultiMessage where
  getCPtr (MultiMessage msg) = msg
instance FMessage SingleFMessage where
  getCFPtr (SingleFMessage msg _) = msg
instance FMessage MultiFMessage where
  getCFPtr (MultiFMessage msg) = msg

instance SocketType Pair where nnSocketType = const NN_PAIR
instance Sender Pair
instance Receiver Pair
instance HasRawSocket Pair

instance SocketType Pub where nnSocketType = const NN_PUB
instance Sender Pub

instance SocketType Sub where nnSocketType = const NN_SUB
instance Receiver Sub

instance SocketType Surveyor where nnSocketType = const NN_SURVEYOR
instance Sender Surveyor
instance Receiver Surveyor
instance HasRawSocket Surveyor

instance SocketType Respondent where nnSocketType = const NN_RESPONDENT
instance Sender Respondent
instance Receiver Respondent
instance HasRawSocket Respondent

instance SocketType Req where nnSocketType = const NN_REQ
instance Sender Req
instance Receiver Req
instance HasRawSocket Req

instance SocketType Rep where nnSocketType = const NN_REP
instance Sender Rep
instance Receiver Rep
instance HasRawSocket Rep

instance SocketType Bus where nnSocketType = const NN_BUS
instance Sender Bus
instance Receiver Bus
instance HasRawSocket Bus

instance SocketType Pull where nnSocketType = const NN_PULL
instance Receiver Pull
instance HasRawSocket Pull

instance SocketType Push where nnSocketType = const NN_PUSH
instance Sender Push
instance HasRawSocket Push


withEither :: (Monad m) => (i -> m o) -> Either e i -> m (Either e o)
withEither r = either (return . Left) (return . Right <=< r)

-- | Create a new nanomsg socket. Socket must be close explicitely. It is safer to use 'withSocket' See official documentation nn_socket(3)

socket :: SocketType a => a -> IO (Either Error (Socket a))
socket t = liftM (fmap Socket) $ nnSocket AF_SP (nnSocketType t)  

-- | Create a raw nanomsg socket. Socket must be close explicitely. It is safer to use 'withSocket'
xsocket :: HasRawSocket a => a -> IO (Either Error (Socket a))
xsocket t = liftM (fmap Socket) $ nnSocket AF_SP_RAW (nnSocketType t)  

-- | Close a socket. Return Nothing if close successfully. See official documentation nn_close(3)
close :: Socket a -> IO(Maybe Error)
close (Socket s) = nnClose s 

-- | Run an action within a new nano socket. The socket is close after the action terminate.
withSocket :: SocketType a => a -> (Socket a -> IO b) -> IO (Either Error b)
withSocket t r = bracket (socket t) (withEither close) (withEither r) 

-- | Same as 'withSocket' but for raw socket.
withXSocket :: HasRawSocket a => a -> (Socket a -> IO b) -> IO (Either Error b)
withXSocket t r = bracket (xsocket t) (withEither close) (withEither r) 


-- | Bind the socket to a given address. See official documentation nn_bind(3)
bind :: Socket a -> String -> IO (Either Error (EndPoint a))
bind (Socket s) add = liftM (fmap (EndPoint s)) $ nnBind s add
-- | Connect the socket to a given address. See official documentation nn_connect(3)
connect :: Socket a -> String -> IO (Either Error (EndPoint a))
connect (Socket s) add = liftM (fmap (EndPoint s)) $ nnConnect s add

-- | Shutdown an endpoint of a socket. For unbinding or unconnect your socket from its endpoints. See official documentation nn_shutdown(3)
shutdown :: EndPoint a -> IO (Maybe Error)
shutdown (EndPoint s ep) = nnShutdown s ep

-- | Device see nn_device(3)
device :: Socket a -> Socket b -> IO (Maybe Error)
device (Socket s1) (Socket s2) = nnDevice s1 s2

-- | Send the given ByteString over the socket. See official documentation nn_send(3)
--   The bytestring is send in a safe way with memory copy.
send :: Sender a => Socket a -> [Flag] -> BS.ByteString -> IO (Either Error Int)
send (Socket s) fls val = BS.useAsCStringLen val (\(cs,l) -> nnSend' s (castPtr cs) l fls)

-- | Variation of send using nanomsg allocate
send' :: Sender a => Socket a -> [Flag] -> BS.ByteString -> IO (Either Error Int)
send' (Socket s) fls (PS pbs pof ple) = do
  p' <- nnAllocmsg (ple - pof) 0
  case p' of 
    Left e  -> return $ Left e
    Right p -> do
      withForeignPtr pbs $ \pbs' -> copyBytes p (castPtr pbs') (ple - pof) -- TODO unsafe use as cstring then poke in memory??
      si' <- nnSendDyn s p fls
      case si' of
        Left _ -> nnFreemsg p >> return si' -- The fact that we free might be a problem if send already free it (send free the buffer when sending)
        Right _ -> return si'


-- | Send the given ByteString over the socket. See official documentation nn_send(3)
-- The bytestring is directly send in an unsafe way.
-- Might be safe (No reason for nanomsg api to touch this buffer)
usend :: Sender a => Socket a -> [Flag] -> BS.ByteString -> IO (Either Error Int)
usend (Socket s) fls val = unsafeUseAsCStringLen val (\(cs,l) -> nnSend' s (castPtr cs) l fls)


-- | Receive a 'ByteString' from socket.  See official documentation nn_send(3)
-- For fix size receive (non dynamic allocation), please use nnRecv or nnRecv' from C.Nanomsg
-- NanoFreeMsg is called when the bytestring is garbage collected. No memory copy.
-- Might be safe (No reason for nanomsg api to touch this buffer)
ureceive :: Receiver a => Socket a -> [Flag] -> IO (Either Error BS.ByteString)
ureceive (Socket sock) fls = do
  (sr, v) <- nnRecvDyn sock fls
  case sr of 
    Left e  -> return $ Left e
    Right s -> return $ Right $ PS (castForeignPtr v) 0 s


-- | Receive a 'ByteString' from socket.  See official documentation nn_send(3)
-- For fix size receive (non dynamic allocation), please use nnRecv or nnRecv' from C.Nanomsg
-- The C memory is freed immediatly after being copied to the bytestring (a bit safer than 'receive')
receive :: Receiver a => Socket a -> [Flag] -> IO (Either Error BS.ByteString)
receive (Socket sock) fls = do
  (sr, v) <- nnRecvDyn' sock fls
  case sr of 
    Left e  -> return $ Left e
    Right s -> do
      bs <- BS.packCStringLen (castPtr v, s)
      nnFreemsg v
      return $ Right bs

-- | Send the given Message over the socket. See official documentation nn_sendmsg(3)
sendMsg :: (Sender a, Message b) => Socket a -> [Flag] -> b -> IO (Either Error Int)
sendMsg (Socket s) fls msg = nnSendmsg s (getCPtr msg) fls
sendFMsg :: (Sender a, FMessage b) => Socket a -> [Flag] -> b -> IO (Either Error Int)
sendFMsg (Socket s) fls msg = nnSendfmsg s (getCFPtr msg) fls


-- | Same as 'receiveSingleMsg' but memory management is done by foreignPointer (at gc no freeMsg required). 
receiveSingleFMsg :: Receiver a => Socket a -> [Flag] -> IO (Either Error SingleFMessage)
receiveSingleFMsg (Socket sock) fls = do
   ptR <- mallocForeignPtr :: IO (ForeignPtr (Ptr ())) -- any ptr TODO in api alloca or foreignfree
   let iovecR = NNFIoVec (castForeignPtr ptR) (fromIntegral nN_MSG)
   fnullPtr <- newForeignPtr finalizerFree nullPtr  
   let msghdrRec = NNFMsgHdr [iovecR] 1 fnullPtr 0
   sr <- nnRecvfmsg sock msghdrRec fls
   case sr of 
    Left e  -> return $ Left e
    Right s -> return $ Right (SingleFMessage msghdrRec s)

-- | Receive a Message from the to socket. See official documentation nn_recvmsg(3) 
-- It uses dynamic size messages
receiveSingleMsg :: Receiver a => Socket a -> [Flag] -> IO (Either Error SingleMessage)
receiveSingleMsg (Socket sock) fls = do
   msghdrRec <- singleMsgInit Nothing
   sr <- nnRecvmsg sock msghdrRec fls
   case sr of 
    Left e  -> return $ Left e
    Right s -> return $ Right (SingleMessage msghdrRec s)

singleMsgInit :: Maybe (Ptr ()) -> IO NNMsgHdr
singleMsgInit mp = do
   ptR <- malloc :: IO (Ptr (Ptr ()))
   let iovecR = NNIoVec (castPtr ptR) (fromIntegral nN_MSG)
   iovecAR <- malloc :: IO (Ptr NNIoVec) -- only one element array
   poke iovecAR iovecR
   maybe (return ()) (poke ptR) mp
   return $ NNMsgHdr (castPtr iovecAR) 1 nullPtr 0
 
-- | Receive a Message from the to socket. See official documentation nn_recvmsg(3)
-- A list of size for each parts is used to allocate memory buffer.
receiveMultiMsg :: Receiver a => Socket a -> [Flag] -> [Int] -> IO (Either Error MultiMessage)
receiveMultiMsg (Socket sock) fls sizes = do
   -- TODO move this code in c interface
   let nbvec = length sizes
   iovecR <- mallocArray nbvec
   iovecR' <- mapM (\s -> mallocArray s >>= (\a -> return (NNIoVec a (fromIntegral s)))) sizes
   pokeArray iovecR iovecR'
   let msghdrRec = NNMsgHdr (castPtr iovecR) (fromIntegral nbvec) nullPtr 0
   sr <- nnRecvmsg sock msghdrRec fls
   case sr of 
    Left e  -> return $ Left e
    Right _ -> return $ Right (MultiMessage msghdrRec)


-- | Sames as 'receiveMultiMsg' but with Foreign pointer Messages
receiveMultiFMsg :: Receiver a => Socket a -> [Flag] -> [Int] -> IO (Either Error MultiFMessage)
receiveMultiFMsg (Socket sock) fls sizes = do
   -- TODO move this code in c interface
   let nbvec = length sizes
   iovecR <- mapM (\s -> mallocForeignPtrArray s >>= (\a -> return (NNFIoVec a (fromIntegral s)))) sizes
   fnullPtr <- newForeignPtr finalizerFree nullPtr
   let msghdrRec = NNFMsgHdr iovecR (fromIntegral nbvec) fnullPtr 0
   sr <- nnRecvfmsg sock msghdrRec fls
   case sr of 
    Left e  -> return $ Left e
    Right _ -> return $ Right (MultiFMessage msghdrRec)


-- | initiate multipart message from bytestrings
-- to work with a list.
newMultimessage :: [ByteString] -> IO MultiMessage
newMultimessage bs = do
   let nbvec = length bs
   iovecR  <- mallocArray nbvec
   iovecR' <- mapM (\(PS pbs pof ple) -> do
     let l = ple - pof
     p <- mallocArray (fromIntegral l)
     withForeignPtr pbs (\pbs' -> copyBytes p (castPtr pbs') (ple - pof))
     return $ NNIoVec p $ fromIntegral l
     ) bs
   pokeArray iovecR iovecR'
   return $ MultiMessage $ NNMsgHdr (castPtr iovecR) (fromIntegral nbvec) nullPtr 0
 
newMultifmessage :: [ByteString] -> IO MultiFMessage
newMultifmessage bs = do
   let nbvec = length bs
   iovecR <- mapM (\(PS pbs pof ple) -> do
     let l = ple - pof
     p <- mallocForeignPtrArray (fromIntegral l)
     withForeignPtr pbs (\pbs' -> withForeignPtr p (\p' -> copyBytes p' (castPtr pbs') (ple - pof)))
     return $ NNFIoVec p $ fromIntegral l
     ) bs
   fnullPtr <- newForeignPtr finalizerFree nullPtr
   return $ MultiFMessage $ NNFMsgHdr iovecR (fromIntegral nbvec) fnullPtr 0


newMultimessage' :: (Storable m) => [m] -> IO MultiMessage
newMultimessage' ms = do
   let nbvec = length ms
   iovecR  <- mallocArray nbvec
   iovecR'  <- initMultimessage' ms
   pokeArray iovecR iovecR'
   return $ MultiMessage $ NNMsgHdr (castPtr iovecR) (fromIntegral nbvec) nullPtr 0

initMultimessage' :: (Storable m) => [m] -> IO [NNIoVec]
initMultimessage' = mapM (\s -> do
     p <- mallocArray $ sizeOf s
     poke p s
     return $ NNIoVec (castPtr p) $ fromIntegral $ sizeOf s
     )

initMultifmessage' :: (Storable m) => [m] -> IO [NNFIoVec]
initMultifmessage' = mapM (\s -> do
     p <- mallocForeignPtrArray $ sizeOf s
     withForeignPtr p (`poke` s)
     return $ NNFIoVec (castForeignPtr p) $ fromIntegral $ sizeOf s
     )

newMultifmessage' :: (Storable m) => [m] -> IO MultiFMessage
newMultifmessage' ms = do
   let nbvec = length ms
   iovecR <- initMultifmessage' ms
   fnullPtr <- newForeignPtr finalizerFree nullPtr
   return $ MultiFMessage $ NNFMsgHdr iovecR (fromIntegral nbvec) fnullPtr 0

-- | initiate singlepart message from a bytestring
newSinglemessage :: ByteString -> IO (Either Error SingleMessage)
newSinglemessage (PS pbs pof ple) = do
   -- write bs in ptR
   let l = ple - pof
   p' <- nnAllocmsg l 0
   case p' of
     Left e -> return $ Left e
     Right p -> do
       msghdr <- singleMsgInit $ Just p
       withForeignPtr pbs (\pbs' -> copyBytes p (castPtr pbs') l)
       return $ Right $ SingleMessage msghdr l

newSinglefmessage :: ByteString -> IO (Either Error SingleFMessage)
newSinglefmessage (PS pbs pof ple) = do
   ptR <- mallocForeignPtr :: IO (ForeignPtr (Ptr ()))
   let iovecR = NNFIoVec (castForeignPtr ptR) (fromIntegral nN_MSG)
   let l = ple - pof
   p' <- nnAllocmsg l 0
   case p' of
     Left e -> return $ Left e
     Right p -> do
       withForeignPtr ptR (`poke` p)
       withForeignPtr pbs (\pbs' -> copyBytes p (castPtr pbs') l)
       fnullPtr <- newForeignPtr finalizerFree nullPtr
       return $ Right $ SingleFMessage (NNFMsgHdr [iovecR] 1 fnullPtr 0) l

newSinglemessage' :: (Storable m) => m -> IO (Either Error SingleMessage)
newSinglemessage' m = do
   let l = sizeOf m
   p' <- nnAllocmsg l 0
   case p' of
     Left e -> return $ Left e
     Right p -> do
       nnmsghdr <- singleMsgInit $ Just p
       poke (castPtr p) m
       return $ Right $ SingleMessage nnmsghdr l

newSinglefmessage' :: (Storable m) => m -> IO (Either Error SingleFMessage)
newSinglefmessage' m = do
   ptR <- mallocForeignPtr :: IO (ForeignPtr (Ptr ()))
   let iovecR = NNFIoVec (castForeignPtr ptR) (fromIntegral nN_MSG)
   let l = sizeOf m
   p' <- nnAllocmsg l 0
   case p' of
     Left e -> return $ Left e
     Right p -> do
       withForeignPtr ptR (`poke` p)
       poke (castPtr p) m
       fnullPtr <- newForeignPtr finalizerFree nullPtr
       return $ Right $ SingleFMessage (NNFMsgHdr [iovecR] 1 fnullPtr 0) l

-- | add message part
-- It involves reallocating array memory so it should only
-- be used for resending a message with extra information
addMultimessage :: [ByteString] -> MultiMessage -> IO MultiMessage
addMultimessage bs (MultiMessage (NNMsgHdr vs vl cs cl)) = do
  let l = length bs
  let nvl = vl + fromIntegral l
  nvs <- reallocArray vs l
  ios <- mapM (\(PS pbs pof ple) -> do
     let l' = ple - pof
     p <- mallocArray (fromIntegral l)
     withForeignPtr pbs (\pbs' -> copyBytes p (castPtr pbs') (ple - pof))
     return $ NNIoVec p $ fromIntegral l'
     ) bs
  pokeArray (nvs `plusPtr` fromIntegral vl) ios
  return $ MultiMessage $ NNMsgHdr nvs nvl cs cl

addMultimessage' :: (Storable m) => [m] -> MultiMessage -> IO MultiMessage
addMultimessage' ms (MultiMessage (NNMsgHdr vs vl cs cl)) = do
  let l = length ms
  let nvl = vl + fromIntegral l
  nvs <- reallocArray vs l
  ios <- initMultimessage' ms
  pokeArray (nvs `plusPtr` fromIntegral vl) ios
  return $ MultiMessage $ NNMsgHdr nvs nvl cs cl

addMultifmessage :: [ByteString] -> MultiFMessage -> IO MultiFMessage
addMultifmessage bs (MultiFMessage (NNFMsgHdr vs vl cs cl)) = do
  let l = length bs
  let nvl = vl + fromIntegral l
  nvs <- mapM (\(PS pbs pof ple) -> do
     let l' = ple - pof
     p <- mallocForeignPtrArray (fromIntegral l)
     withForeignPtr pbs (\pbs' -> withForeignPtr p (\p' -> copyBytes p' (castPtr pbs') (ple - pof)))
     return $ NNFIoVec p $ fromIntegral l'
     ) bs
  return $ MultiFMessage $ NNFMsgHdr (vs ++ nvs) nvl cs cl

addMultifmessage' :: (Storable m) => [m] -> MultiFMessage -> IO MultiFMessage
addMultifmessage' ms (MultiFMessage (NNFMsgHdr vs vl cs cl)) = do
  let l = length ms
  let nvl = vl + fromIntegral l
  nvs <- initMultifmessage' ms
  return $ MultiFMessage $ NNFMsgHdr (vs ++ nvs) nvl cs cl

-- | Finalizer for Message.
freeMsg :: Message b => b -> IO () -- TODO a with for bracketing
freeMsg msg = freeNNMsghdr $ getCPtr msg where
  freeNNMsghdr (NNMsgHdr vs vl cs _) = do
    unless (cs == nullPtr) $ free cs
    ios <- peekArray (fromIntegral vl) vs
    mapM_ (\(NNIoVec base iol) ->
      if iol == fromIntegral nN_MSG then do
        nnFreemsg (castPtr base)
        return () -- ignore error
      else
        free base
      ) ios
    free vs 

-- | Force term. Use nn_term to close all socket. Use with care.
forceTerm :: IO()
forceTerm = nnTerm

getEnumOption :: (AllSocketOptions a, AllLevelOptions b, Enum e) => Socket s -> b -> a -> IO (Either Error e)
getEnumOption a b c = (return . fmap toEnum) =<< getIntOption a b c -- TODO find nicer syntax
 
getIntOption :: (AllSocketOptions a, AllLevelOptions b) => Socket s -> b -> a -> IO (Either Error Int)
getIntOption (Socket sock) level option = do
   let cintsize = sizeOf (undefined :: CInt)
   (err, val, _) <- allocaBytes cintsize (\p -> nnGetsockopt sock level option p cintsize)
   case err of
     Just e -> return $ Left e
     Nothing -> do
       v <- peek (castPtr val :: Ptr CInt)
       return $ Right (fromIntegral v)
 
setIntOption :: (AllSocketOptions a, AllLevelOptions b) => Socket s -> b -> a -> Int -> IO (Maybe Error)
setIntOption (Socket s) b a v = alloca (\p -> poke p (fromIntegral v :: CInt) >> nnSetsockopt s b a (castPtr p) (sizeOf v))
 
getStringOption :: (AllSocketOptions a, AllLevelOptions b) => Socket s -> b -> a -> IO (Either Error String)
getStringOption (Socket sock) level option = do
   let cintsize = sizeOf (undefined :: CInt)
   (err, val, size) <- allocaBytes cintsize (\p -> nnGetsockopt sock level option p cintsize)
   case err of
     Just e -> return $ Left e
     Nothing -> do
       v <- peekCStringLen (castPtr val, size)
       return $ Right v

setStringOption :: (AllSocketOptions a, AllLevelOptions b) => Socket s -> b -> a -> String -> IO (Maybe Error)
setStringOption (Socket s) b a v = withCString v (\cs -> nnSetsockopt s b a (castPtr cs) (sizeOf cs))

-- | Please refer to nn_getsockopt(3) Manual Page
getDomain :: Socket s -> IO (Either Error AddressFamilies)
getDomain s = getEnumOption s NN_SOL_SOCKET NN_DOMAIN
 
-- | Please refer to nn_getsockopt(3) Manual Page
getProtocol :: Socket s -> IO (Either Error NnProtocol)
getProtocol s = getEnumOption s NN_SOL_SOCKET NN_PROTOCOL

-- | Please refer to nn_getsockopt(3) Manual Page
getLinger :: Socket s -> IO (Either Error Int)
getLinger s = getIntOption s NN_SOL_SOCKET NN_LINGER

-- | Please refer to nn_setsockopt(3) Manual Page
setLinger :: Socket s -> Int -> IO (Maybe Error)
setLinger s = setIntOption s NN_SOL_SOCKET NN_LINGER

-- | Please refer to nn_getsockopt(3) Manual Page
getSendBuffer :: (Sender s) => Socket s -> IO (Either Error Int)
getSendBuffer s = getIntOption s NN_SOL_SOCKET NN_SNDBUF

-- | Please refer to nn_setsockopt(3) Manual Page
setSendBuffer :: (Sender s) => Socket s -> Int -> IO (Maybe Error)
setSendBuffer s = setIntOption s NN_SOL_SOCKET NN_SNDBUF

-- | Please refer to nn_getsockopt(3) Manual Page
getReceiveBuffer :: (Receiver s) => Socket s -> IO (Either Error Int)
getReceiveBuffer s = getIntOption s NN_SOL_SOCKET NN_RCVBUF

-- | Please refer to nn_setsockopt(3) Manual Page
setReceiveBuffer :: (Receiver s) => Socket s -> Int -> IO (Maybe Error)
setReceiveBuffer s = setIntOption s NN_SOL_SOCKET NN_RCVBUF

-- | Please refer to nn_getsockopt(3) Manual Page
getSendTimeout :: (Sender s) => Socket s -> IO (Either Error Int)
getSendTimeout s = getIntOption s NN_SOL_SOCKET NN_SNDTIMEO

-- | Please refer to nn_setsockopt(3) Manual Page
setSendTimeout :: (Sender s) => Socket s -> Int -> IO (Maybe Error)
setSendTimeout s = setIntOption s NN_SOL_SOCKET NN_SNDTIMEO

-- | Please refer to nn_getsockopt(3) Manual Page
getReceiveTimeout :: (Receiver s) => Socket s -> IO (Either Error Int)
getReceiveTimeout s = getIntOption s NN_SOL_SOCKET NN_RCVTIMEO

-- | Please refer to nn_setsockopt(3) Manual Page
setReceiveTimeout :: (Receiver s) => Socket s -> Int -> IO (Maybe Error)
setReceiveTimeout s = setIntOption s NN_SOL_SOCKET NN_RCVTIMEO

-- | Please refer to nn_getsockopt(3) Manual Page
getReconnectInterval :: Socket s -> IO (Either Error Int)
getReconnectInterval s = getIntOption s NN_SOL_SOCKET NN_RECONNECT_IVL

-- | Please refer to nn_setsockopt(3) Manual Page
setReconnectInterval :: Socket s -> Int -> IO (Maybe Error)
setReconnectInterval s = setIntOption s NN_SOL_SOCKET NN_RECONNECT_IVL

-- | Please refer to nn_getsockopt(3) Manual Page
getReconnectIntervalMax :: Socket s -> IO (Either Error Int)
getReconnectIntervalMax s = getIntOption s NN_SOL_SOCKET NN_RECONNECT_IVL_MAX

-- | Please refer to nn_setsockopt(3) Manual Page
setReconnectIntervalMax :: Socket s -> Int -> IO (Maybe Error)
setReconnectIntervalMax s = setIntOption s NN_SOL_SOCKET NN_RECONNECT_IVL_MAX

-- | Please refer to nn_getsockopt(3) Manual Page
getSendPrio :: (Sender s) => Socket s -> IO (Either Error Int)
getSendPrio s = getIntOption s NN_SOL_SOCKET NN_SNDPRIO

-- | Please refer to nn_setsockopt(3) Manual Page
setSendPrio :: (Sender s) => Socket s -> Int -> IO (Maybe Error)
setSendPrio s = setIntOption s NN_SOL_SOCKET NN_SNDPRIO

-- | Please refer to nn_getsockopt(3) Manual Page
getIPV4ONLY :: Socket s -> IO (Either Error Bool)
getIPV4ONLY s = (return . fmap toEnum) =<< getIntOption s NN_SOL_SOCKET NN_IPV4ONLY

-- | Please refer to nn_setsockopt(3) Manual Page
setIPV4ONLY :: Socket s -> Bool -> IO (Maybe Error)
setIPV4ONLY s v = setIntOption s NN_SOL_SOCKET NN_IPV4ONLY $ fromEnum v

-- | Please refer to nn_reqrep(7) Manual Page
getReqResendInterval :: Socket Req -> IO (Either Error Int)
getReqResendInterval s = getIntOption s NN_REQ NN_REQ_RESEND_IVL

-- | Please refer to nn_reqrep(7) Manual Page
setReqResendInterval :: Socket Req -> Int -> IO (Maybe Error)
setReqResendInterval s = setIntOption s NN_REQ NN_REQ_RESEND_IVL

-- | Please refer to nn_pubsub(7) Manual Page
getSubSubscribe :: Socket Req -> IO (Either Error String)
getSubSubscribe s = getStringOption s NN_SUB NN_SUB_SUBSCRIBE

-- | Please refer to nn_pubsub(7) Manual Page
setSubSubscribe :: Socket Sub -> String -> IO (Maybe Error)
setSubSubscribe s = setStringOption s NN_SUB NN_SUB_SUBSCRIBE

-- | Please refer to nn_pubsub(7) Manual Page
getSubUnsubscribe :: Socket Req -> IO (Either Error String)
getSubUnsubscribe s = getStringOption s NN_SUB NN_SUB_UNSUBSCRIBE

-- | Please refer to nn_pubsub(7) Manual Page
setSubUnsubscribe :: Socket Sub -> String -> IO (Maybe Error)
setSubUnsubscribe s = setStringOption s NN_SUB NN_SUB_UNSUBSCRIBE

-- | Please refer to nn_survey(7) Manual Page
getSurveyorDeadline :: Socket Surveyor -> IO (Either Error Int)
getSurveyorDeadline s = getIntOption s  NN_SURVEYOR NN_SURVEYOR_DEADLINE 

-- | Please refer to nn_survey(7) Manual Page
setSurveyorDeadline :: Socket Surveyor -> Int -> IO (Maybe Error)
setSurveyorDeadline s = setIntOption s NN_SURVEYOR NN_SURVEYOR_DEADLINE


