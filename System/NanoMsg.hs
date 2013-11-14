{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
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
    Socket(..)
  , EndPoint(..)
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
import Control.Monad(unless)
import Foreign.C.String(withCStringLen,peekCStringLen)
import Control.Exception(Exception,throw,catch,SomeException)
import Text.Printf(printf)
import Data.Typeable(Typeable)

-- | Type for all socket
newtype Socket a = Socket NnSocket


-- | Type for flag
type Flag = SndRcvFlags

-- | Type for messages with only one part
-- SingleMessage are restricted to only one part
-- MultiMessage are not
-- FMessage got automatic memory management (using ForeignPointer)
-- Using FMessages is not always safer, and could be problematics (memory release depends upon garbage collector so it is not deterministic).
-- TODO this and socket type should be rewritten to use gadt
-- For message hdr, current nanomsg (alpha) store only dynamic length header -> we add a small implementation for static length which explain some strange serialization to nN_MSG
data SingleMessage = SingleMessage NNMsgHdr Int
data SingleFMessage = SingleFMessage NNFMsgHdr Int 
data MultiMessage = MultiMessage NNMsgHdr 
data MultiFMessage = MultiFMessage NNFMsgHdr 

-- | Type for Endpoint of sockets
data EndPoint a = EndPoint NnSocket NnEndPoint


-- | NanoError, error through exception in API, encapsulate NnError
-- when using the native 0MQ API, such as error number and message.
-- Similar to Zmq3 Api Error
data NanoError = NanoError {
    errno   :: NnError -- ^ C Error enum.
  , source  :: String -- ^ Source where this error originates from.
  , message :: String  -- ^ Actual error message.
  } deriving (Eq, Ord, Typeable)

instance Show NanoError where
    show e = printf "NanoError { errno = %s, source = \"%s\", message = \"%s\" }"
                (show (errno e)) (source e) (message e)

instance Exception NanoError

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


--withEither :: (Monad m) => (i -> m o) -> Either e i -> m (Either e o)
--withEither r = either (return . Left) (return . Right <=< r)

throwError :: String -> NnError -> IO a
throwError from e = do
  m <- catch  (nnStrerror e) ((\_ -> return "") :: SomeException -> IO String)
  throw $ NanoError e from m

-- read either and throw exception with error
eitherError :: String -> Either NnError a -> IO a
eitherError from (Left e)  = throwError from e
eitherError _    (Right a) = return a

-- read maybe containing an error and throw exception with error
maybeError :: String -> Maybe NnError -> IO ()
maybeError from (Just e) = throwError from e
maybeError _    Nothing  = return ()

-- | Create a new nanomsg socket. Socket must be close explicitely. It is safer to use 'withSocket' See official documentation nn_socket(3)
-- Throws NanoError
socket :: SocketType a => a -> IO (Socket a)
socket t = nnSocket AF_SP (nnSocketType t) >>= eitherError "socket" >>= return . Socket

-- | Create a raw nanomsg socket. Socket must be close explicitely. It is safer to use 'withSocket'
-- Throws NanoError
xsocket :: HasRawSocket a => a -> IO (Socket a)
xsocket t = nnSocket AF_SP (nnSocketType t) >>= eitherError "xsocket" >>= return . Socket

-- | Close a socket. Return Nothing if close successfully. See official documentation nn_close(3)
-- Throws NanoError
close :: Socket a -> IO ()
close (Socket s) = nnClose s >>= maybeError "close" 

-- | Run an action within a new nano socket. The socket is close after the action terminate.
-- Throws NanoError
withSocket :: SocketType a => a -> (Socket a -> IO b) -> IO b
withSocket t r = bracket (socket t) close r

-- | Same as 'withSocket' but for raw socket.
-- Throws NanoError
withXSocket :: HasRawSocket a => a -> (Socket a -> IO b) -> IO b
withXSocket t r = bracket (xsocket t) close r

-- | Bind the socket to a given address. See official documentation nn_bind(3)
-- Throws NanoError
bind :: Socket a -> String -> IO (EndPoint a)
bind (Socket s) add = nnBind s add >>= eitherError "bind" >>= return . EndPoint s

-- | Connect the socket to a given address. See official documentation nn_connect(3)
-- Throws NanoError
connect :: Socket a -> String -> IO (EndPoint a)
connect (Socket s) add = nnConnect s add >>= eitherError "connect" >>= return . EndPoint s

-- | Shutdown an endpoint of a socket. For unbinding or unconnect your socket from its endpoints. See official documentation nn_shutdown(3)
-- Throws NanoError
shutdown :: EndPoint a -> IO ()
shutdown (EndPoint s ep) = nnShutdown s ep >>= maybeError "shutdown"

-- | Device see nn_device(3)
-- Throws NanoError
device :: Socket a -> Socket b -> IO ()
device (Socket s1) (Socket s2) = nnDevice s1 s2 >>= maybeError "device"

-- | Send the given ByteString over the socket. See official documentation nn_send(3)
--   The bytestring is send in a safe way with memory copy.
-- Throws NanoError
send :: Sender a => Socket a -> [Flag] -> BS.ByteString -> IO Int
send (Socket s) fls val = BS.useAsCStringLen val (\(cs,l) -> nnSend' s (castPtr cs) l fls) >>= eitherError "send"

-- | Variation of send using nanomsg allocate
-- Throws NanoError
send' :: Sender a => Socket a -> [Flag] -> BS.ByteString -> IO Int
send' (Socket s) fls (PS pbs pof ple) = do
  p <- nnAllocmsg (ple - pof) 0 >>= eitherError "send' nnAllocmsg"
  withForeignPtr pbs $ \pbs' -> copyBytes p (castPtr pbs') (ple - pof) -- TODO unsafe use as cstring then poke in memory??
  si' <- nnSendDyn s p fls
  case si' of
    Left e -> nnFreemsg p >> throwError "send'" e -- The fact that we free might be a problem if send already free it (send free the buffer when sending) TODO further testing
    Right si -> return si


-- | Send the given ByteString over the socket. See official documentation nn_send(3)
-- The bytestring is directly send in an unsafe way.
-- Might be safe (No reason for nanomsg api to touch this buffer)
-- Throws NanoError
usend :: Sender a => Socket a -> [Flag] -> BS.ByteString -> IO Int
usend (Socket s) fls val = unsafeUseAsCStringLen val (\(cs,l) -> nnSend' s (castPtr cs) l fls) >>= eitherError "usend"


-- | Receive a 'ByteString' from socket.  See official documentation nn_send(3)
-- For fix size receive (non dynamic allocation), please use nnRecv or nnRecv' from C.Nanomsg
-- NanoFreeMsg is called when the bytestring is garbage collected. No memory copy.
-- Might be safe (No reason for nanomsg api to touch this buffer)
-- Throws NanoError
ureceive :: Receiver a => Socket a -> [Flag] -> IO BS.ByteString
ureceive (Socket sock) fls = do
  (sr, v) <- nnRecvDyn sock fls
  eitherError "ureceive" sr >>= return . PS (castForeignPtr v) 0


-- | Receive a 'ByteString' from socket.  See official documentation nn_send(3)
-- For fix size receive (non dynamic allocation), please use nnRecv or nnRecv' from C.Nanomsg
-- The C memory is freed immediatly after being copied to the bytestring (a bit safer than 'receive')
-- Throws NanoError
receive :: Receiver a => Socket a -> [Flag] -> IO BS.ByteString
receive (Socket sock) fls = do
  (sr, v) <- nnRecvDyn' sock fls
  s  <- eitherError "receive" sr
  bs <- BS.packCStringLen (castPtr v, s)
  nnFreemsg v >>= maybeError "receive freemsg"
  return bs

-- | Send the given Message over the socket. See official documentation nn_sendmsg(3)
-- Throws NanoError
sendMsg :: (Sender a, Message b) => Socket a -> [Flag] -> b -> IO Int
sendMsg (Socket s) fls msg = nnSendmsg s (getCPtr msg) fls >>= eitherError "sendMsg"
sendFMsg :: (Sender a, FMessage b) => Socket a -> [Flag] -> b -> IO Int
sendFMsg (Socket s) fls msg = nnSendfmsg s (getCFPtr msg) fls >>= eitherError "sendFMsg"


-- | Same as 'receiveSingleMsg' but memory management is done by foreignPointer (at gc no freeMsg required). 
-- Throws NanoError
receiveSingleFMsg :: Receiver a => Socket a -> [Flag] -> IO SingleFMessage
receiveSingleFMsg (Socket sock) fls = do
   ptR <- mallocForeignPtr :: IO (ForeignPtr (Ptr ())) -- any ptr TODO in api alloca or foreignfree
   let iovecR = NNFIoVec (castForeignPtr ptR) (fromIntegral nN_MSG)
   fnullPtr <- newForeignPtr finalizerFree nullPtr  
   let msghdrRec = NNFMsgHdr [iovecR] 1 fnullPtr 0
   nnRecvfmsg sock msghdrRec fls >>= eitherError "receiveSingleFMsg" >>= return . SingleFMessage msghdrRec

-- | Receive a Message from the to socket. See official documentation nn_recvmsg(3) 
-- It uses dynamic size messages
-- Throws NanoError
receiveSingleMsg :: Receiver a => Socket a -> [Flag] -> IO SingleMessage
receiveSingleMsg (Socket sock) fls = do
   msghdrRec <- singleMsgInit Nothing
   nnRecvmsg sock msghdrRec fls >>= eitherError "receiveSingleMsg" >>= return . SingleMessage msghdrRec

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
-- Throws NanoError
receiveMultiMsg :: Receiver a => Socket a -> [Flag] -> [Int] -> IO MultiMessage
receiveMultiMsg (Socket sock) fls sizes = do
   -- TODO move this code in c interface
   let nbvec = length sizes
   iovecR <- mallocArray nbvec
   iovecR' <- mapM (\s -> mallocArray s >>= (\a -> return (NNIoVec a (fromIntegral s)))) sizes
   pokeArray iovecR iovecR'
   let msghdrRec = NNMsgHdr (castPtr iovecR) (fromIntegral nbvec) nullPtr 0
   _ <- nnRecvmsg sock msghdrRec fls >>= eitherError "receiveMultiMsg"
   return $ MultiMessage msghdrRec


-- | Sames as 'receiveMultiMsg' but with Foreign pointer Messages
-- Throws NanoError
receiveMultiFMsg :: Receiver a => Socket a -> [Flag] -> [Int] -> IO MultiFMessage
receiveMultiFMsg (Socket sock) fls sizes = do
   -- TODO move this code in c interface
   let nbvec = length sizes
   iovecR <- mapM (\s -> mallocForeignPtrArray s >>= (\a -> return (NNFIoVec a (fromIntegral s)))) sizes
   fnullPtr <- newForeignPtr finalizerFree nullPtr
   let msghdrRec = NNFMsgHdr iovecR (fromIntegral nbvec) fnullPtr 0
   _ <- nnRecvfmsg sock msghdrRec fls >>= eitherError "receiveMultiFMsg"
   return $ MultiFMessage msghdrRec


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
-- Throws NanoError
newSinglemessage :: ByteString -> IO SingleMessage
newSinglemessage (PS pbs pof ple) = do
   -- write bs in ptR
   let l = ple - pof
   p <- nnAllocmsg l 0 >>= eitherError "newSinglemessage"
   msghdr <- singleMsgInit $ Just p
   withForeignPtr pbs (\pbs' -> copyBytes p (castPtr pbs') l)
   return $ SingleMessage msghdr l

-- | initiate singlepart fmessage (memory manage through foreign pointers) from a bytestring
-- Throws NanoError
newSinglefmessage :: ByteString -> IO SingleFMessage
newSinglefmessage (PS pbs pof ple) = do
   ptR <- mallocForeignPtr :: IO (ForeignPtr (Ptr ()))
   let iovecR = NNFIoVec (castForeignPtr ptR) (fromIntegral nN_MSG)
   let l = ple - pof
   p <- nnAllocmsg l 0 >>= eitherError "newSinglefmessage"
   withForeignPtr ptR (`poke` p)
   withForeignPtr pbs (\pbs' -> copyBytes p (castPtr pbs') l)
   fnullPtr <- newForeignPtr finalizerFree nullPtr
   return $ SingleFMessage (NNFMsgHdr [iovecR] 1 fnullPtr 0) l

newSinglemessage' :: (Storable m) => m -> IO SingleMessage
newSinglemessage' m = do
   let l = sizeOf m
   p <- nnAllocmsg l 0 >>= eitherError "newSinglemessage'"
   nnmsghdr <- singleMsgInit $ Just p
   poke (castPtr p) m
   return $ SingleMessage nnmsghdr l

newSinglefmessage' :: (Storable m) => m -> IO SingleFMessage
newSinglefmessage' m = do
   ptR <- mallocForeignPtr :: IO (ForeignPtr (Ptr ()))
   let iovecR = NNFIoVec (castForeignPtr ptR) (fromIntegral nN_MSG)
   let l = sizeOf m
   p <- nnAllocmsg l 0 >>= eitherError "newSinglefmessage'"
   withForeignPtr ptR (`poke` p)
   poke (castPtr p) m
   fnullPtr <- newForeignPtr finalizerFree nullPtr
   return $ SingleFMessage (NNFMsgHdr [iovecR] 1 fnullPtr 0) l

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
        _ <- nnFreemsg (castPtr base)
        return () -- ignore error
      else
        free base
      ) ios
    free vs 

-- | Force term. Use nn_term to close all socket. Use with care.
forceTerm :: IO()
forceTerm = nnTerm

-- Throws NanoError
getEnumOption :: (AllSocketOptions a, AllLevelOptions b, Enum n) => Socket s -> b -> a -> IO n
getEnumOption a b c = (return . toEnum) =<< getIntOption a b c -- TODO find nicer syntax
-- eitherError 
-- Throws NanoError
getIntOption :: (AllSocketOptions a, AllLevelOptions b) => Socket s -> b -> a -> IO Int
getIntOption (Socket sock) level option = do
   let cintsize = sizeOf (undefined :: CInt)
   (err, val, _) <- allocaBytes cintsize (\p -> nnGetsockopt sock level option p cintsize)
   maybeError ("getIntOption " ++ show level ++ " " ++ show option) err
   peek (castPtr val :: Ptr CInt) >>= return . fromIntegral
 
-- Throws NanoError
setIntOption :: (AllSocketOptions a, AllLevelOptions b) => Socket s -> b -> a -> Int -> IO ()
setIntOption (Socket s) b a v = alloca (\p -> poke p (fromIntegral v :: CInt) >> nnSetsockopt s b a (castPtr p) (sizeOf (undefined :: CInt))) >>= maybeError ("setIntOption " ++ show b ++ " " ++ show a ++ " " ++ show v)

-- Throws NanoError
getStringOption :: (AllSocketOptions a, AllLevelOptions b) => Socket s -> b -> a -> IO String
getStringOption (Socket sock) level option = do
   let cintsize = sizeOf (undefined :: CInt)
   (err, val, size) <- allocaBytes cintsize (\p -> nnGetsockopt sock level option p cintsize)
   maybeError ("getStringOption " ++ show level ++ " " ++ show option) err
   peekCStringLen (castPtr val, size)

-- Throws NanoError
setStringOption :: (AllSocketOptions a, AllLevelOptions b) => Socket s -> b -> a -> String -> IO ()
setStringOption (Socket s) b a v = withCStringLen v (\(cs,l) -> nnSetsockopt s b a (castPtr cs) l) >>= maybeError ("setStringOption " ++ show b ++ " " ++ show a ++ " " ++ show v)

-- | Please refer to nn_getsockopt(3) Manual Page
-- Throws NanoError
getDomain :: Socket s -> IO AddressFamilies
getDomain s = getEnumOption s NN_SOL_SOCKET NN_DOMAIN
 
-- | Please refer to nn_getsockopt(3) Manual Page
-- Throws NanoError
getProtocol :: Socket s -> IO NnProtocol
getProtocol s = getEnumOption s NN_SOL_SOCKET NN_PROTOCOL

-- | Please refer to nn_getsockopt(3) Manual Page
-- Throws NanoError
getLinger :: Socket s -> IO Int
getLinger s = getIntOption s NN_SOL_SOCKET NN_LINGER

-- | Please refer to nn_setsockopt(3) Manual Page
-- Throws NanoError
setLinger :: Socket s -> Int -> IO ()
setLinger s = setIntOption s NN_SOL_SOCKET NN_LINGER

-- | Please refer to nn_getsockopt(3) Manual Page
-- Throws NanoError
getSendBuffer :: (Sender s) => Socket s -> IO Int
getSendBuffer s = getIntOption s NN_SOL_SOCKET NN_SNDBUF

-- | Please refer to nn_setsockopt(3) Manual Page
-- Throws NanoError
setSendBuffer :: (Sender s) => Socket s -> Int -> IO ()
setSendBuffer s = setIntOption s NN_SOL_SOCKET NN_SNDBUF

-- | Please refer to nn_getsockopt(3) Manual Page
-- Throws NanoError
getReceiveBuffer :: (Receiver s) => Socket s -> IO Int
getReceiveBuffer s = getIntOption s NN_SOL_SOCKET NN_RCVBUF

-- | Please refer to nn_setsockopt(3) Manual Page
-- Throws NanoError
setReceiveBuffer :: (Receiver s) => Socket s -> Int -> IO ()
setReceiveBuffer s = setIntOption s NN_SOL_SOCKET NN_RCVBUF

-- | Please refer to nn_getsockopt(3) Manual Page
-- Throws NanoError
getSendTimeout :: (Sender s) => Socket s -> IO Int
getSendTimeout s = getIntOption s NN_SOL_SOCKET NN_SNDTIMEO

-- | Please refer to nn_setsockopt(3) Manual Page
-- Throws NanoError
setSendTimeout :: (Sender s) => Socket s -> Int -> IO ()
setSendTimeout s = setIntOption s NN_SOL_SOCKET NN_SNDTIMEO

-- | Please refer to nn_getsockopt(3) Manual Page
-- Throws NanoError
getReceiveTimeout :: (Receiver s) => Socket s -> IO Int
getReceiveTimeout s = getIntOption s NN_SOL_SOCKET NN_RCVTIMEO

-- | Please refer to nn_setsockopt(3) Manual Page
-- Throws NanoError
setReceiveTimeout :: (Receiver s) => Socket s -> Int -> IO ()
setReceiveTimeout s = setIntOption s NN_SOL_SOCKET NN_RCVTIMEO

-- | Please refer to nn_getsockopt(3) Manual Page
-- Throws NanoError
getReconnectInterval :: Socket s -> IO Int
getReconnectInterval s = getIntOption s NN_SOL_SOCKET NN_RECONNECT_IVL

-- | Please refer to nn_setsockopt(3) Manual Page
-- Throws NanoError
setReconnectInterval :: Socket s -> Int -> IO ()
setReconnectInterval s = setIntOption s NN_SOL_SOCKET NN_RECONNECT_IVL

-- | Please refer to nn_getsockopt(3) Manual Page
-- Throws NanoError
getReconnectIntervalMax :: Socket s -> IO Int
getReconnectIntervalMax s = getIntOption s NN_SOL_SOCKET NN_RECONNECT_IVL_MAX

-- | Please refer to nn_setsockopt(3) Manual Page
-- Throws NanoError
setReconnectIntervalMax :: Socket s -> Int -> IO ()
setReconnectIntervalMax s = setIntOption s NN_SOL_SOCKET NN_RECONNECT_IVL_MAX

-- | Please refer to nn_getsockopt(3) Manual Page
-- Throws NanoError
getSendPrio :: (Sender s) => Socket s -> IO Int
getSendPrio s = getIntOption s NN_SOL_SOCKET NN_SNDPRIO

-- | Please refer to nn_setsockopt(3) Manual Page
-- Throws NanoError
setSendPrio :: (Sender s) => Socket s -> Int -> IO ()
setSendPrio s = setIntOption s NN_SOL_SOCKET NN_SNDPRIO

-- | Please refer to nn_getsockopt(3) Manual Page
-- Throws NanoError
getIPV4ONLY :: Socket s -> IO Bool
getIPV4ONLY s = getIntOption s NN_SOL_SOCKET NN_IPV4ONLY >>= return . toEnum

-- | Please refer to nn_setsockopt(3) Manual Page
-- Throws NanoError
setIPV4ONLY :: Socket s -> Bool -> IO ()
setIPV4ONLY s v = setIntOption s NN_SOL_SOCKET NN_IPV4ONLY $ fromEnum v

-- | Please refer to nn_reqrep(7) Manual Page
-- Throws NanoError
getReqResendInterval :: Socket Req -> IO Int
getReqResendInterval s = getIntOption s NN_REQ NN_REQ_RESEND_IVL

-- | Please refer to nn_reqrep(7) Manual Page
-- Throws NanoError
setReqResendInterval :: Socket Req -> Int -> IO ()
setReqResendInterval s = setIntOption s NN_REQ NN_REQ_RESEND_IVL

-- | Please refer to nn_pubsub(7) Manual Page
-- Note at this time (nanomsg alpha), not implemented in C api
-- Throws NanoError
getSubSubscribe :: Socket Sub -> IO String
getSubSubscribe s = getStringOption s NN_SUB NN_SUB_SUBSCRIBE

-- | Please refer to nn_pubsub(7) Manual Page
-- Throws NanoError
setSubSubscribe :: Socket Sub -> String -> IO ()
setSubSubscribe s = setStringOption s NN_SUB NN_SUB_SUBSCRIBE

-- | Please refer to nn_pubsub(7) Manual Page
-- Note at this time (nanomsg alpha), not implemented in C api
-- Throws NanoError
getSubUnsubscribe :: Socket Sub -> IO String
getSubUnsubscribe s = getStringOption s NN_SUB NN_SUB_UNSUBSCRIBE

-- | Please refer to nn_pubsub(7) Manual Page
-- Throws NanoError
setSubUnsubscribe :: Socket Sub -> String -> IO ()
setSubUnsubscribe s = setStringOption s NN_SUB NN_SUB_UNSUBSCRIBE

-- | Please refer to nn_survey(7) Manual Page
-- Throws NanoError
getSurveyorDeadline :: Socket Surveyor -> IO Int
getSurveyorDeadline s = getIntOption s  NN_SURVEYOR NN_SURVEYOR_DEADLINE 

-- | Please refer to nn_survey(7) Manual Page
-- Throws NanoError
setSurveyorDeadline :: Socket Surveyor -> Int -> IO ()
setSurveyorDeadline s = setIntOption s NN_SURVEYOR NN_SURVEYOR_DEADLINE


