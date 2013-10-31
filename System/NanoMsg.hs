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
  , Flag(..)
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

  , device
  -- * Socket general Options
  
  -- * Socket specific options
  
  
  ) where
import System.NanoMsg.C.NanoMsg
import System.NanoMsg.C.NanoMsgStruct
import Control.Exception(bracket)
import Control.Applicative((<$>))
import Control.Monad((<=<))
import qualified Data.ByteString as BS
import Data.ByteString.Internal(ByteString(PS))
import Foreign.Ptr(castPtr)
import Foreign(withForeignPtr)
import Foreign.Marshal.Utils(copyBytes)
import Foreign.Ptr(plusPtr)
import Data.ByteString.Unsafe(unsafeUseAsCStringLen)
import Foreign.ForeignPtr(castForeignPtr)

-- | Type for all socket
newtype Socket a = Socket NnSocket

-- | Type for nanoError
type Error = NnError

-- | Type for flag
type Flag = SndRcvFlags

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
socket t = nnSocket AF_SP (nnSocketType t) >>= return . fmap Socket

-- | Create a raw nanomsg socket. Socket must be close explicitely. It is safer to use 'withSocket'
xsocket :: HasRawSocket a => a -> IO (Either Error (Socket a))
xsocket t = nnSocket AF_SP_RAW (nnSocketType t) >>= return . fmap Socket

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
bind (Socket s) add = nnBind s add >>= return . fmap (EndPoint s)

-- | Connect the socket to a given address. See official documentation nn_connect(3)
connect :: Socket a -> String -> IO (Either Error (EndPoint a))
connect (Socket s) add = nnConnect s add >>= return . fmap (EndPoint s)

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
send' (Socket s) fls val@(PS pbs pof ple) = do
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
      bs <- BS.packCStringLen ((castPtr v), s)
      nnFreemsg v
      return $ Right bs


