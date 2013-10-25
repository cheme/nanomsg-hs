-- GENERATED by C->Haskell Compiler, version 0.16.5 Crystal Seed, 24 Jan 2009 (Haskell)
-- Edit the ORIGNAL .chs file instead!


{-# LINE 1 "System/NanoMsg/C/NanoMsg.chs" #-}{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.NanoMsg.C.NanoMsg where
-- | This module aims at exposing nanomsg function directly to haskell, no api construct except the use of ForeignFreePointers. Function specific documentation is therefore the official nanomsg documentation. An exception is also done for error handling, here we use maybe or either. Note that it is a c2hs module and that returning type of function may contain function parameter when they may be changed (a tuple with firstly function return type then all parameters in order).
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad(liftM)
import Foreign.Ptr(FunPtr(..))
import qualified Control.Monad as M
import Control.Monad((>=>))
import Control.Monad((<=<))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as U


-- #include "nanomsg/transport.h"
-- #include "nanomsg/protocol.h"
data NnError = ENOTSUP
             | ENOMEM
             | EPROTONOSUPPORT
             | ENAMETOOLONG
             | ENODEV
             | ENOBUFS
             | ENETDOWN
             | EADDRINUSE
             | EADDRNOTAVAIL
             | ECONNREFUSED
             | EINPROGRESS
             | ENOTSOCK
             | EAFNOSUPPORT
             | EPROTO
             | EAGAIN
             | EBADF
             | EINVAL
             | EINTR
             | EMFILE
             | EFAULT
             | EACCESS
             | ENETRESET
             | ENETUNREACH
             | EHOSTUNREACH
             | ENOTCONN
             | EMSGSIZE
             | ETIMEDOUT
             | ECONNABORTED
             | ECONNRESET
             | ENOPROTOOPT
             | EISCONN
             | ETERM
             | EFSM
             deriving (Eq,Ord,Show)
instance Enum NnError where
  fromEnum ENOTSUP = 95
  fromEnum ENOMEM = 12
  fromEnum EPROTONOSUPPORT = 93
  fromEnum ENAMETOOLONG = 36
  fromEnum ENODEV = 19
  fromEnum ENOBUFS = 105
  fromEnum ENETDOWN = 100
  fromEnum EADDRINUSE = 98
  fromEnum EADDRNOTAVAIL = 99
  fromEnum ECONNREFUSED = 111
  fromEnum EINPROGRESS = 115
  fromEnum ENOTSOCK = 88
  fromEnum EAFNOSUPPORT = 97
  fromEnum EPROTO = 71
  fromEnum EAGAIN = 11
  fromEnum EBADF = 9
  fromEnum EINVAL = 22
  fromEnum EINTR = 4
  fromEnum EMFILE = 24
  fromEnum EFAULT = 14
  fromEnum EACCESS = 156384729
  fromEnum ENETRESET = 102
  fromEnum ENETUNREACH = 101
  fromEnum EHOSTUNREACH = 113
  fromEnum ENOTCONN = 107
  fromEnum EMSGSIZE = 90
  fromEnum ETIMEDOUT = 110
  fromEnum ECONNABORTED = 103
  fromEnum ECONNRESET = 104
  fromEnum ENOPROTOOPT = 92
  fromEnum EISCONN = 106
  fromEnum ETERM = 156384765
  fromEnum EFSM = 156384766

  toEnum 95 = ENOTSUP
  toEnum 12 = ENOMEM
  toEnum 93 = EPROTONOSUPPORT
  toEnum 36 = ENAMETOOLONG
  toEnum 19 = ENODEV
  toEnum 105 = ENOBUFS
  toEnum 100 = ENETDOWN
  toEnum 98 = EADDRINUSE
  toEnum 99 = EADDRNOTAVAIL
  toEnum 111 = ECONNREFUSED
  toEnum 115 = EINPROGRESS
  toEnum 88 = ENOTSOCK
  toEnum 97 = EAFNOSUPPORT
  toEnum 71 = EPROTO
  toEnum 11 = EAGAIN
  toEnum 9 = EBADF
  toEnum 22 = EINVAL
  toEnum 4 = EINTR
  toEnum 24 = EMFILE
  toEnum 14 = EFAULT
  toEnum 156384729 = EACCESS
  toEnum 102 = ENETRESET
  toEnum 101 = ENETUNREACH
  toEnum 113 = EHOSTUNREACH
  toEnum 107 = ENOTCONN
  toEnum 90 = EMSGSIZE
  toEnum 110 = ETIMEDOUT
  toEnum 103 = ECONNABORTED
  toEnum 104 = ECONNRESET
  toEnum 92 = ENOPROTOOPT
  toEnum 106 = EISCONN
  toEnum 156384765 = ETERM
  toEnum 156384766 = EFSM
  toEnum unmatched = error ("NnError.toEnum: Cannot match " ++ show unmatched)
 -- TODO instance show with nnStrerror (but with code to be able to reverse) 

data AddressFamilies = AF_SP
                     | AF_SP_RAW
                     deriving (Eq,Ord,Show)
instance Enum AddressFamilies where
  fromEnum AF_SP = 1
  fromEnum AF_SP_RAW = 2

  toEnum 1 = AF_SP
  toEnum 2 = AF_SP_RAW
  toEnum unmatched = error ("AddressFamilies.toEnum: Cannot match " ++ show unmatched)

{-# LINE 69 "System/NanoMsg/C/NanoMsg.chs" #-}

data NnTransport = NN_IPC
                 | NN_INPROC
                 | NN_TCP
                 deriving (Eq,Ord,Show)
instance Enum NnTransport where
  fromEnum NN_IPC = (-2)
  fromEnum NN_INPROC = (-1)
  fromEnum NN_TCP = (-3)

  toEnum (-2) = NN_IPC
  toEnum (-1) = NN_INPROC
  toEnum (-3) = NN_TCP
  toEnum unmatched = error ("NnTransport.toEnum: Cannot match " ++ show unmatched)

{-# LINE 74 "System/NanoMsg/C/NanoMsg.chs" #-}
-- TODO associate to their protocol
data ProtocolFamilies = NN_PROTO_PUBSUB
                      | NN_PROTO_BUS
                      | NN_PROTO_PAIR
                      | NN_PROTO_PIPELINE
                      | NN_PROTO_REQREP
                      | NN_PROTO_SURVEY
                      deriving (Eq,Ord,Show)
instance Enum ProtocolFamilies where
  fromEnum NN_PROTO_PUBSUB = 2
  fromEnum NN_PROTO_BUS = 7
  fromEnum NN_PROTO_PAIR = 1
  fromEnum NN_PROTO_PIPELINE = 5
  fromEnum NN_PROTO_REQREP = 3
  fromEnum NN_PROTO_SURVEY = 6

  toEnum 2 = NN_PROTO_PUBSUB
  toEnum 7 = NN_PROTO_BUS
  toEnum 1 = NN_PROTO_PAIR
  toEnum 5 = NN_PROTO_PIPELINE
  toEnum 3 = NN_PROTO_REQREP
  toEnum 6 = NN_PROTO_SURVEY
  toEnum unmatched = error ("ProtocolFamilies.toEnum: Cannot match " ++ show unmatched)

{-# LINE 82 "System/NanoMsg/C/NanoMsg.chs" #-}

data NnProtocol = NN_PUB
                | NN_SUB
                | NN_BUS
                | NN_PAIR
                | NN_PUSH
                | NN_PULL
                | NN_REQ
                | NN_REP
                | NN_SURVEYOR
                | NN_RESPONDENT
                deriving (Eq,Ord,Show)
instance Enum NnProtocol where
  fromEnum NN_PUB = 32
  fromEnum NN_SUB = 33
  fromEnum NN_BUS = 112
  fromEnum NN_PAIR = 16
  fromEnum NN_PUSH = 80
  fromEnum NN_PULL = 81
  fromEnum NN_REQ = 48
  fromEnum NN_REP = 49
  fromEnum NN_SURVEYOR = 96
  fromEnum NN_RESPONDENT = 97

  toEnum 32 = NN_PUB
  toEnum 33 = NN_SUB
  toEnum 112 = NN_BUS
  toEnum 16 = NN_PAIR
  toEnum 80 = NN_PUSH
  toEnum 81 = NN_PULL
  toEnum 48 = NN_REQ
  toEnum 49 = NN_REP
  toEnum 96 = NN_SURVEYOR
  toEnum 97 = NN_RESPONDENT
  toEnum unmatched = error ("NnProtocol.toEnum: Cannot match " ++ show unmatched)

{-# LINE 94 "System/NanoMsg/C/NanoMsg.chs" #-}


-- | those constants might be useless, if they are of any use we might consider using c2hs #const instead
data NNConstants = NN_SOCKADDR_MAX
                 deriving (Eq,Ord,Show)
instance Enum NNConstants where
  fromEnum NN_SOCKADDR_MAX = 128

  toEnum 128 = NN_SOCKADDR_MAX
  toEnum unmatched = error ("NNConstants.toEnum: Cannot match " ++ show unmatched)

{-# LINE 100 "System/NanoMsg/C/NanoMsg.chs" #-}

data SolSocket = NN_SOL_SOCKET
               deriving (Eq,Ord,Show)
instance Enum SolSocket where
  fromEnum NN_SOL_SOCKET = 0

  toEnum 0 = NN_SOL_SOCKET
  toEnum unmatched = error ("SolSocket.toEnum: Cannot match " ++ show unmatched)

{-# LINE 104 "System/NanoMsg/C/NanoMsg.chs" #-}


data SocketOptions = NN_LINGER
                   | NN_SNDBUF
                   | NN_RCVBUF
                   | NN_SNDTIMEO
                   | NN_RCVTIMEO
                   | NN_RECONNECT_IVL
                   | NN_RECONNECT_IVL_MAX
                   | NN_SNDPRIO
                   | NN_SNDFD
                   | NN_RCVFD
                   | NN_IPV4ONLY
                   deriving (Eq,Ord,Show)
instance Enum SocketOptions where
  fromEnum NN_LINGER = 1
  fromEnum NN_SNDBUF = 2
  fromEnum NN_RCVBUF = 3
  fromEnum NN_SNDTIMEO = 4
  fromEnum NN_RCVTIMEO = 5
  fromEnum NN_RECONNECT_IVL = 6
  fromEnum NN_RECONNECT_IVL_MAX = 7
  fromEnum NN_SNDPRIO = 8
  fromEnum NN_SNDFD = 10
  fromEnum NN_RCVFD = 11
  fromEnum NN_IPV4ONLY = 14

  toEnum 1 = NN_LINGER
  toEnum 2 = NN_SNDBUF
  toEnum 3 = NN_RCVBUF
  toEnum 4 = NN_SNDTIMEO
  toEnum 5 = NN_RCVTIMEO
  toEnum 6 = NN_RECONNECT_IVL
  toEnum 7 = NN_RECONNECT_IVL_MAX
  toEnum 8 = NN_SNDPRIO
  toEnum 10 = NN_SNDFD
  toEnum 11 = NN_RCVFD
  toEnum 14 = NN_IPV4ONLY
  toEnum unmatched = error ("SocketOptions.toEnum: Cannot match " ++ show unmatched)

{-# LINE 119 "System/NanoMsg/C/NanoMsg.chs" #-}

data SocketReadOptions = NN_DOMAIN
                       | NN_PROTOCOL
                       deriving (Eq,Ord,Show)
instance Enum SocketReadOptions where
  fromEnum NN_DOMAIN = 12
  fromEnum NN_PROTOCOL = 13

  toEnum 12 = NN_DOMAIN
  toEnum 13 = NN_PROTOCOL
  toEnum unmatched = error ("SocketReadOptions.toEnum: Cannot match " ++ show unmatched)

{-# LINE 124 "System/NanoMsg/C/NanoMsg.chs" #-}
 
data PubSubOptions = NN_REQ_RESEND_IVL
                   deriving (Eq,Ord,Show)
instance Enum PubSubOptions where
  fromEnum NN_REQ_RESEND_IVL = 1

  toEnum 1 = NN_REQ_RESEND_IVL
  toEnum unmatched = error ("PubSubOptions.toEnum: Cannot match " ++ show unmatched)

{-# LINE 128 "System/NanoMsg/C/NanoMsg.chs" #-}

data SurveyOptions = NN_SURVEYOR_DEADLINE
                   deriving (Eq,Ord,Show)
instance Enum SurveyOptions where
  fromEnum NN_SURVEYOR_DEADLINE = 1

  toEnum 1 = NN_SURVEYOR_DEADLINE
  toEnum unmatched = error ("SurveyOptions.toEnum: Cannot match " ++ show unmatched)

{-# LINE 132 "System/NanoMsg/C/NanoMsg.chs" #-}


data ReqRepOptions = NN_SUB_SUBSCRIBE
                   | NN_SUB_UNSUBSCRIBE
                   deriving (Eq,Ord,Show)
instance Enum ReqRepOptions where
  fromEnum NN_SUB_SUBSCRIBE = 1
  fromEnum NN_SUB_UNSUBSCRIBE = 2

  toEnum 1 = NN_SUB_SUBSCRIBE
  toEnum 2 = NN_SUB_UNSUBSCRIBE
  toEnum unmatched = error ("ReqRepOptions.toEnum: Cannot match " ++ show unmatched)

{-# LINE 138 "System/NanoMsg/C/NanoMsg.chs" #-}

data TcpOptions = NN_TCP_NODELAY
                deriving (Eq,Ord,Show)
instance Enum TcpOptions where
  fromEnum NN_TCP_NODELAY = 1

  toEnum 1 = NN_TCP_NODELAY
  toEnum unmatched = error ("TcpOptions.toEnum: Cannot match " ++ show unmatched)

{-# LINE 142 "System/NanoMsg/C/NanoMsg.chs" #-}


-- | internal only
class (Enum a) => AllSocketOptions a
instance AllSocketOptions ReqRepOptions
instance AllSocketOptions PubSubOptions
instance AllSocketOptions SocketOptions
instance AllSocketOptions TcpOptions
instance AllSocketOptions SocketReadOptions
-- | internal only
class (Enum a) => AllLevelOptions a
instance AllLevelOptions NnProtocol
instance AllLevelOptions SolSocket
instance AllLevelOptions NnTransport




data SndRcvFlags = NN_DONTWAIT
                 deriving (Eq,Ord,Show)
instance Enum SndRcvFlags where
  fromEnum NN_DONTWAIT = 1

  toEnum 1 = NN_DONTWAIT
  toEnum unmatched = error ("SndRcvFlags.toEnum: Cannot match " ++ show unmatched)

{-# LINE 162 "System/NanoMsg/C/NanoMsg.chs" #-}

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = fromIntegral . fromEnum

peekInt :: Ptr CInt -> IO Int
peekInt = (liftM fromIntegral) . peek

peekInteger :: (Storable a, Integral a) => Ptr a -> IO Integer
peekInteger = (liftM toInteger) . peek


foreignFree :: Ptr a -> IO(ForeignPtr a)
foreignFree = newForeignPtr finalizerFree

foreignVoid :: Ptr () -> IO(ForeignPtr ())
foreignVoid = newForeignPtr finalizerFree

foreignFreeMsg :: Ptr () -> IO(Either NnError (ForeignPtr ()))
foreignFreeMsg =  either (return . Left) (return . Right <=< newForeignPtr nnFunPtrFreeMsg) <=< errorFromNewPointer

cPackCString = C.packCString
ucPackCString = C.packCString . castPtr
uPackCString = U.unsafePackCString
uuPackCString = U.unsafePackCString . castPtr

errorFromRetCode :: CInt -> IO(Maybe NnError)
errorFromRetCode r = if r < 0 then nnErrno >>= return . Just else return Nothing

errorFromNewPointer :: Ptr () -> IO(Either NnError (Ptr ()))
errorFromNewPointer ptr = if ptr == nullPtr then nnErrno >>= return . Left else return $ Right ptr


errorFromLength :: CInt -> IO(Either NnError Integer)
errorFromLength r = if r < 0 then nnErrno >>= return . Left else (return . Right . toInteger) r

errorFromSocket :: CInt -> IO(Either NnError NnSocket)
errorFromSocket r = if r < 0 then nnErrno >>= return . Left else (return . Right . NnSocket) r

errorFromEndPoint :: CInt -> IO(Either NnError NnEndPoint)
errorFromEndPoint r = if r < 0 then nnErrno >>= return . Left else (return . Right . NnEndPoint) r


nnErrno :: IO ((NnError))
nnErrno =
  nnErrno'_ >>= \res ->
  let {res' = cIntToEnum res} in
  return (res')
{-# LINE 208 "System/NanoMsg/C/NanoMsg.chs" #-}
-- Mostly useless as c2hs uses macro, so no mapping from Int values to Enum
nnSymbol :: (Int) -> IO ((String), (Int))
nnSymbol a1 =
  let {a1' = fromIntegral a1} in 
  alloca $ \a2' -> 
  nnSymbol'_ a1' a2' >>= \res ->
  peekCString res >>= \res' ->
  peekInt  a2'>>= \a2'' -> 
  return (res', a2'')
{-# LINE 210 "System/NanoMsg/C/NanoMsg.chs" #-}
nnStrerror :: (NnError) -> IO ((String))
nnStrerror a1 =
  let {a1' = cIntFromEnum a1} in 
  nnStrerror'_ a1' >>= \res ->
  peekCString res >>= \res' ->
  return (res')
{-# LINE 211 "System/NanoMsg/C/NanoMsg.chs" #-}

-- TODO code from sample to catch errors
dummy' = nnTerm
{-# LINE 214 "System/NanoMsg/C/NanoMsg.chs" #-}

-- type of allocation is transport dependant -- see transport implementation --> for haskell api link it to the transport used -- TODO (tricky??)
nnAllocmsg' :: (Integer) -> (Int) -> IO ((Either NnError (ForeignPtr ())))
nnAllocmsg' a1 a2 =
  let {a1' = fromIntegral a1} in 
  let {a2' = fromIntegral a2} in 
  nnAllocmsg''_ a1' a2' >>= \res ->
  foreignFreeMsg res >>= \res' ->
  return (res')
{-# LINE 217 "System/NanoMsg/C/NanoMsg.chs" #-}
nnAllocmsg :: (Integer) -> (Int) -> IO ((Either NnError (Ptr ())))
nnAllocmsg a1 a2 =
  let {a1' = fromIntegral a1} in 
  let {a2' = fromIntegral a2} in 
  nnAllocmsg'_ a1' a2' >>= \res ->
  errorFromNewPointer res >>= \res' ->
  return (res')
{-# LINE 218 "System/NanoMsg/C/NanoMsg.chs" #-}

-- do not use if nnAllocmsg' used
nnFreemsg :: (Ptr ()) -> IO ((Maybe NnError))
nnFreemsg a1 =
  let {a1' = id a1} in 
  nnFreemsg'_ a1' >>= \res ->
  errorFromRetCode res >>= \res' ->
  return (res')
{-# LINE 221 "System/NanoMsg/C/NanoMsg.chs" #-}

-- not c2hs (to type it explicitly as a funPtr for finalizer) TODO test if discarding result is an issue with linking -- Internal use only 
foreign import ccall "System/NanoMsg/C/NanoMsg.chs.h &nn_freemsg"
   nnFunPtrFreeMsg :: FunPtr (Ptr () -> IO ())

cmsgFirsthdr :: (NnMsghdr) -> IO ((NnCmsghdr))
cmsgFirsthdr a1 =
  let {a1' = fromMsghdr a1} in 
  cmsgFirsthdr'_ a1' >>= \res ->
  let {res' = toCmsghdr res} in
  return (res')
{-# LINE 227 "System/NanoMsg/C/NanoMsg.chs" #-}
cmsgNxthdr :: (NnMsghdr) -> (NnCmsghdr) -> IO ((Maybe NnCmsghdr))
cmsgNxthdr a1 a2 =
  let {a1' = fromMsghdr a1} in 
  let {a2' = fromCmsghdr a2} in 
  cmsgNxthdr'_ a1' a2' >>= \res ->
  let {res' = maybeCmsg res} in
  return (res')
{-# LINE 228 "System/NanoMsg/C/NanoMsg.chs" #-}
-- | use of byteString for char * here. Note that Bytestring is copied. -- not we use unsigned char and do a cast ptr : ByteString should not be use with unicode.
cmsgData :: (NnCmsghdr) -> IO ((ByteString))
cmsgData a1 =
  let {a1' = fromCmsghdr a1} in 
  cmsgData'_ a1' >>= \res ->
  ucPackCString res >>= \res' ->
  return (res')
{-# LINE 230 "System/NanoMsg/C/NanoMsg.chs" #-}
-- | unsafe version for efficiency. To test but might be ok.
cmsgData' :: (NnCmsghdr) -> IO ((ByteString))
cmsgData' a1 =
  let {a1' = fromCmsghdr a1} in 
  cmsgData''_ a1' >>= \res ->
  uuPackCString res >>= \res' ->
  return (res')
{-# LINE 232 "System/NanoMsg/C/NanoMsg.chs" #-}
-- | might not be pure in the future but given current nanomsg implementation it is ok
cmsgLen :: (Integer) -> (Integer)
cmsgLen a1 =
  let {a1' = fromIntegral a1} in 
  let {res = cmsgLen'_ a1'} in
  let {res' = toInteger res} in
  (res')
{-# LINE 234 "System/NanoMsg/C/NanoMsg.chs" #-}
cmsgLen' :: (Integer) -> IO ((Integer))
cmsgLen' a1 =
  let {a1' = fromIntegral a1} in 
  cmsgLen''_ a1' >>= \res ->
  let {res' = toInteger res} in
  return (res')
{-# LINE 235 "System/NanoMsg/C/NanoMsg.chs" #-}
-- | might not be pure in the future but given current nanomsg implementation it is ok
cmsgSpace :: (Integer) -> (Integer)
cmsgSpace a1 =
  let {a1' = fromIntegral a1} in 
  let {res = cmsgSpace'_ a1'} in
  let {res' = toInteger res} in
  (res')
{-# LINE 237 "System/NanoMsg/C/NanoMsg.chs" #-}
cmsgSpace' :: (Integer) -> IO ((Integer))
cmsgSpace' a1 =
  let {a1' = fromIntegral a1} in 
  cmsgSpace''_ a1' >>= \res ->
  let {res' = toInteger res} in
  return (res')
{-# LINE 238 "System/NanoMsg/C/NanoMsg.chs" #-}


newtype NnSocket = NnSocket CInt deriving (Eq, Show)
socketToCInt (NnSocket s) = s
-- TODO enum for domain???
nnSocket :: (AddressFamilies) -> (NnProtocol) -> IO ((Either NnError NnSocket))
nnSocket a1 a2 =
  let {a1' = cIntFromEnum a1} in 
  let {a2' = cIntFromEnum a2} in 
  nnSocket'_ a1' a2' >>= \res ->
  errorFromSocket res >>= \res' ->
  return (res')
{-# LINE 244 "System/NanoMsg/C/NanoMsg.chs" #-}

nnClose :: (NnSocket) -> IO ((Maybe NnError))
nnClose a1 =
  let {a1' = socketToCInt a1} in 
  nnClose'_ a1' >>= \res ->
  errorFromRetCode res >>= \res' ->
  return (res')
{-# LINE 246 "System/NanoMsg/C/NanoMsg.chs" #-}

nnSetsockopt :: (AllSocketOptions a, AllLevelOptions b) => (NnSocket) -> (b) -> (a) -> (Ptr ()) -> (Integer) -> IO ((Maybe NnError))
nnSetsockopt a1 a2 a3 a4 a5 =
  let {a1' = socketToCInt a1} in 
  let {a2' = cIntFromEnum a2} in 
  let {a3' = cIntFromEnum a3} in 
  let {a4' = id a4} in 
  let {a5' = fromIntegral a5} in 
  nnSetsockopt'_ a1' a2' a3' a4' a5' >>= \res ->
  errorFromRetCode res >>= \res' ->
  return (res')
{-# LINE 248 "System/NanoMsg/C/NanoMsg.chs" #-}

withNullPtr :: (Ptr () -> IO b) -> IO b
withNullPtr r = r nullPtr
-- | handling of values for options out of c api - we do not allocate memory for return value of option -- and do not send size -- should use a stablepointer?? -- to test thoroughly (doc initiate size and pointer which does not make any sense
nnGetsockopt :: (AllSocketOptions a, AllLevelOptions b) => (NnSocket) -> (b) -> (a) -> IO ((Maybe NnError), (Ptr ()), (Integer))
nnGetsockopt a1 a2 a3 =
  let {a1' = socketToCInt a1} in 
  let {a2' = cIntFromEnum a2} in 
  let {a3' = cIntFromEnum a3} in 
  withNullPtr $ \a4' -> 
  alloca $ \a5' -> 
  nnGetsockopt'_ a1' a2' a3' a4' a5' >>= \res ->
  errorFromRetCode res >>= \res' ->
  let {a4'' = id  a4'} in 
  peekInteger  a5'>>= \a5'' -> 
  return (res', a4'', a5'')
{-# LINE 253 "System/NanoMsg/C/NanoMsg.chs" #-}

newtype NnEndPoint = NnEndPoint CInt deriving (Eq, Show)
endPointToCInt (NnEndPoint s) = s
-- TODO bind an address type to avoid address without :// (in api(one per transport) using NN_SOCKADDR_MAX)
nnBind :: (NnSocket) -> (String) -> IO ((Either NnError NnEndPoint))
nnBind a1 a2 =
  let {a1' = socketToCInt a1} in 
  withCString a2 $ \a2' -> 
  nnBind'_ a1' a2' >>= \res ->
  errorFromEndPoint res >>= \res' ->
  return (res')
{-# LINE 258 "System/NanoMsg/C/NanoMsg.chs" #-}

nnConnect :: (NnSocket) -> (String) -> IO ((Either NnError NnEndPoint))
nnConnect a1 a2 =
  let {a1' = socketToCInt a1} in 
  withCString a2 $ \a2' -> 
  nnConnect'_ a1' a2' >>= \res ->
  errorFromEndPoint res >>= \res' ->
  return (res')
{-# LINE 260 "System/NanoMsg/C/NanoMsg.chs" #-}

nnShutdown :: (NnSocket) -> (NnEndPoint) -> IO ((Maybe NnError))
nnShutdown a1 a2 =
  let {a1' = socketToCInt a1} in 
  let {a2' = endPointToCInt a2} in 
  nnShutdown'_ a1' a2' >>= \res ->
  errorFromRetCode res >>= \res' ->
  return (res')
{-# LINE 262 "System/NanoMsg/C/NanoMsg.chs" #-}

-- | type to send not in C (not even storable)
nnSend :: (NnSocket) -> (ForeignPtr ()) -> (Integer) -> (SndRcvFlags) -> IO ((Either NnError Integer))
nnSend a1 a2 a3 a4 =
  let {a1' = socketToCInt a1} in 
  withForeignPtr a2 $ \a2' -> 
  let {a3' = fromIntegral a3} in 
  let {a4' = cIntFromEnum a4} in 
  nnSend'_ a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')
{-# LINE 265 "System/NanoMsg/C/NanoMsg.chs" #-}
-- | not ForeignFree
nnSend' :: (NnSocket) -> (Ptr ()) -> (Integer) -> (SndRcvFlags) -> IO ((Either NnError Integer))
nnSend' a1 a2 a3 a4 =
  let {a1' = socketToCInt a1} in 
  let {a2' = id a2} in 
  let {a3' = fromIntegral a3} in 
  let {a4' = cIntFromEnum a4} in 
  nnSend''_ a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')
{-# LINE 267 "System/NanoMsg/C/NanoMsg.chs" #-}
-- | no foreign (deallocate is managed by nanomq)
nnSendDyn :: (NnSocket) -> (Ptr ()) -> (SndRcvFlags) -> IO ((Either NnError Integer))
nnSendDyn a1 a2 a4 =
  let {a1' = socketToCInt a1} in 
  let {a2' = id a2} in 
  withNnMSG $ \a3' -> 
  let {a4' = cIntFromEnum a4} in 
  nnSendDyn'_ a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')
{-# LINE 269 "System/NanoMsg/C/NanoMsg.chs" #-}

-- TODO fn with foreign does not make too much sense (should be in api)
nnRecvDyn' :: (NnSocket) -> (SndRcvFlags) -> IO ((Either NnError Integer), (Ptr ()))
nnRecvDyn' a1 a4 =
  let {a1' = socketToCInt a1} in 
  withNullPtr $ \a2' -> 
  withNnMSG $ \a3' -> 
  let {a4' = cIntFromEnum a4} in 
  nnRecvDyn''_ a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  let {a2'' = id  a2'} in 
  return (res', a2'')
{-# LINE 272 "System/NanoMsg/C/NanoMsg.chs" #-}
nnRecvDyn :: (NnSocket) -> (SndRcvFlags) -> IO ((Either NnError Integer), (ForeignPtr ()))
nnRecvDyn a1 a4 =
  let {a1' = socketToCInt a1} in 
  withNullPtr $ \a2' -> 
  withNnMSG $ \a3' -> 
  let {a4' = cIntFromEnum a4} in 
  nnRecvDyn'_ a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  foreignVoid  a2'>>= \a2'' -> 
  return (res', a2'')
{-# LINE 273 "System/NanoMsg/C/NanoMsg.chs" #-}

-- TODO fn with foreign does not make too much sense (should be in api)
nnRecv :: (NnSocket) -> (ForeignPtr ()) -> (Integer) -> (SndRcvFlags) -> IO ((Either NnError Integer))
nnRecv a1 a2 a3 a4 =
  let {a1' = socketToCInt a1} in 
  withForeignPtr a2 $ \a2' -> 
  let {a3' = fromIntegral a3} in 
  let {a4' = cIntFromEnum a4} in 
  nnRecv'_ a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')
{-# LINE 276 "System/NanoMsg/C/NanoMsg.chs" #-}
nnRecv' :: (NnSocket) -> (Ptr ()) -> (Integer) -> (SndRcvFlags) -> IO ((Either NnError Integer))
nnRecv' a1 a2 a3 a4 =
  let {a1' = socketToCInt a1} in 
  let {a2' = id a2} in 
  let {a3' = fromIntegral a3} in 
  let {a4' = cIntFromEnum a4} in 
  nnRecv''_ a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')
{-# LINE 277 "System/NanoMsg/C/NanoMsg.chs" #-}


nnSendmsg :: (NnSocket) -> (NnFMsghdr) -> (SndRcvFlags) -> IO ((Either NnError Integer))
nnSendmsg a1 a2 a3 =
  let {a1' = socketToCInt a1} in 
  withFmsghdr a2 $ \a2' -> 
  let {a3' = cIntFromEnum a3} in 
  nnSendmsg'_ a1' a2' a3' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')
{-# LINE 280 "System/NanoMsg/C/NanoMsg.chs" #-}
nnSendmsg' :: (NnSocket) -> (NnMsghdr) -> (SndRcvFlags) -> IO ((Either NnError Integer))
nnSendmsg' a1 a2 a3 =
  let {a1' = socketToCInt a1} in 
  let {a2' = fromMsghdr a2} in 
  let {a3' = cIntFromEnum a3} in 
  nnSendmsg''_ a1' a2' a3' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')
{-# LINE 281 "System/NanoMsg/C/NanoMsg.chs" #-}

--withFmsghdr :: NnFMsghdr -> Ptr()
withFmsghdr f r =  withForeignPtr f (r . castPtr)
-- | Warning value of constant NN_MSG is hardcoded to (-1). Due to restriction on cast with c2hs. TODO use #const in a separate hs2c file or use inline macro to cast to an int (dirty).
withNnMSG a = a (-1)
{- 
* struct nn_cmsghdr *NN_CMSG_FIRSTHDR(struct nn_msghdr *hdr);
NN_CMSG_FIRSTHDR returns a pointer to the first nn_cmsghdr in the control buffer in the supplied nn_msghdr structure. => in struct length level and type -> generic enum other level and type
* struct nn_cmsghdr *NN_CMSG_NXTHDR(struct nn_msghdr *hdr, struct nn_cmsghdr *cmsg);
NN_CMSG_NXTHDR returns the next nn_cmsghdr after the supplied nn_cmsghdr. Returns NULL if there isn’t enough space in the buffer.
* unsigned char *NN_CMSG_DATA(struct nn_cmsghdr *cmsg);
NN_CMSG_DATA returns a pointer to the data associated with supplied nn_cmsghdr. => fuck magic one (nothing like this in the struct plus return unsigned char *) -> pointer + 1 : stupid
* size_t NN_CMSG_SPACE(size_t len);
NN_CMSG_SPACE returns the number of bytes occupied by nn_cmsghdr with payload of the specified length. => fuck magic to use len and return size -> no just a multiplication -> pure function
* size_t NN_CMSG_LEN(size_t len);
NN_CMSG_LEN returns the value to store in the cmsg_len member of the cmsghdr structure, taking into account any necessary alignment. => idem

TODO a function for : Alternatively, to send a buffer allocated by nn_allocmsg(3) function set iov_base to point to the pointer to the buffer and iov_len to NN_MSG constant. In this case a successful call to nn_send will deallocate the buffer. Trying to deallocate it afterwards will result in undefined behaviour. Also, scatter array in nn_msghdr structure can contain only one element in this case. = send

TODO a function for : Alternatively, nanomsg library can allocate the buffer for you. To do so, let the iov_base point to void* variable to receive the buffer and set iov_len to NN_MSG. After successful completion user is responsible for deallocating the message using nn_freemsg(3) function. Gather array in nn_msghdr structure can contain only one element in this case. = receive : not from exemple all must be allocated

TODO single message send fn with allocmsg usage, and strcpy of bytestring? like send example (with work on pointers.
-}


-- | Struct related Code for simplicity and to avoid boilerplate code, this could be refactor in a separate hs2c module with usage of data, or moved in c helper functions.
-- use freeForeignPointer or explicit pointer (with manual free)
type NnFIovec = ForeignPtr (NnIovec)
{-# LINE 309 "System/NanoMsg/C/NanoMsg.chs" #-}
type NnFMsghdr = ForeignPtr (NnMsghdr)
{-# LINE 310 "System/NanoMsg/C/NanoMsg.chs" #-}
type NnFCmsghdr = ForeignPtr (NnCmsghdr)
{-# LINE 311 "System/NanoMsg/C/NanoMsg.chs" #-}
newtype NnIovec = NnIovec (Ptr (NnIovec))
{-# LINE 312 "System/NanoMsg/C/NanoMsg.chs" #-}
newtype NnMsghdr = NnMsghdr (Ptr (NnMsghdr))
{-# LINE 313 "System/NanoMsg/C/NanoMsg.chs" #-}
newtype NnCmsghdr = NnCmsghdr (Ptr (NnCmsghdr))
{-# LINE 314 "System/NanoMsg/C/NanoMsg.chs" #-}
fromMsghdr  (NnMsghdr m)   = castPtr m
fromCmsghdr  (NnCmsghdr m) = castPtr m
fromFIovec  (NnIovec v)   = castPtr v
toMsghdr  = NnMsghdr . castPtr
toCmsghdr = NnCmsghdr . castPtr
toFIovec  = NnIovec . castPtr
maybeCmsg m = if m == nullPtr then Nothing else (Just . toCmsghdr) m 

-- TODO Inline all getter and setter
-- TODO instance show and reverse of show for those struct
-- Terrible boilerplate code -- avoid data usage for struct 
-- Performance related it may be nicer to create c helper functions (we already got a wrapper for macro). (for getter or to implement usecase where those setter - getter are relevant).
iovecGetBase' :: NnIovec -> IO(Ptr())
iovecGetBase' (NnIovec v) = (\ptr -> do {peekByteOff ptr 0 ::IO (Ptr ())}) v
iovecGetBase :: NnFIovec -> IO(Ptr())
iovecGetBase = (`withForeignPtr` (\ptr -> do {peekByteOff ptr 0 ::IO (Ptr ())}))
iovecGetLen'' = (liftM toInteger) . (\ptr -> do {peekByteOff ptr 8 ::IO CULong})
{-# LINE 331 "System/NanoMsg/C/NanoMsg.chs" #-}
iovecGetLen' :: NnIovec -> IO(Integer)
iovecGetLen' (NnIovec v) = iovecGetLen'' v
iovecGetLen :: NnFIovec -> IO(Integer)
iovecGetLen = (`withForeignPtr` iovecGetLen'')
iovecSetLen'' v l = (\ptr val -> do {pokeByteOff ptr 8 (val::CULong)}) v (fromInteger l)
iovecSetLen' :: NnIovec -> Integer -> IO()
iovecSetLen' (NnIovec v) l = iovecSetLen'' v l
iovecSetLen :: NnFIovec -> Integer -> IO()
iovecSetLen p l = withForeignPtr p (`iovecSetLen''` l)
iovecSetBase'' v l = (\ptr val -> do {pokeByteOff ptr 0 (val::(Ptr ()))}) v l
iovecSetBase' :: NnIovec -> Ptr() -> IO()
iovecSetBase' (NnIovec v) l = iovecSetBase'' v l
iovecSetBase :: NnFIovec -> Ptr() -> IO()
iovecSetBase p l = withForeignPtr p (`iovecSetBase''` l)

--msghdrGetIov :: NnFMsghdr -> IO(NnFIovec)
msghdrGetIov'' mh = (\ptr -> do {peekByteOff ptr 0 :: IO(Ptr a)}) mh
msghdrGetIov' :: NnMsghdr -> IO(NnIovec)
msghdrGetIov' (NnMsghdr mh)= liftM NnIovec (msghdrGetIov'' mh)
msghdrGetIov :: NnFMsghdr -> IO(NnFIovec)
msghdrGetIov a = withForeignPtr a msghdrGetIov'' >>= foreignFree
msghdrSetIov'' v l = (\ptr val -> do {pokeByteOff ptr 0 (val::(Ptr NnIovec))}) v l
msghdrSetIov' :: NnMsghdr -> NnIovec -> IO()
msghdrSetIov' (NnMsghdr h) (NnIovec v) = msghdrSetIov'' h v
msghdrSetIov :: NnFMsghdr -> NnFIovec -> IO()
msghdrSetIov h v = withForeignPtr h (\h' -> withForeignPtr v (\v' -> msghdrSetIov'' h' v'))

msghdrGetiovlen'' = (liftM fromIntegral) . (\ptr -> do {peekByteOff ptr 8 ::IO CInt})
{-# LINE 359 "System/NanoMsg/C/NanoMsg.chs" #-}
msghdrGetiovlen' :: NnMsghdr -> IO(Int)
msghdrGetiovlen' (NnMsghdr v) = msghdrGetiovlen'' v
msghdrGetiovlen :: NnFMsghdr -> IO(Int)
msghdrGetiovlen = (`withForeignPtr` msghdrGetiovlen'')
msghdrSetiovlen'' v l = (\ptr val -> do {pokeByteOff ptr 8 (val::CInt)}) v (fromIntegral l)
msghdrSetiovlen' :: NnMsghdr -> Int -> IO()
msghdrSetiovlen' (NnMsghdr h) l = msghdrSetiovlen'' h l
msghdrSetiovlen ::  NnFMsghdr -> Int -> IO()
msghdrSetiovlen h v = withForeignPtr h (`msghdrSetiovlen''` v)

msghdrGetcontrol'' = (\ptr -> do {peekByteOff ptr 16 ::IO (Ptr ())})
{-# LINE 370 "System/NanoMsg/C/NanoMsg.chs" #-}
msghdrGetcontrol' :: NnMsghdr -> IO(Ptr())
msghdrGetcontrol' (NnMsghdr v) = msghdrGetcontrol'' v
msghdrGetcontrol :: NnFMsghdr -> IO(Ptr())
msghdrGetcontrol = (`withForeignPtr` msghdrGetcontrol'')
msghdrSetcontrol'' v l = (\ptr val -> do {pokeByteOff ptr 16 (val::(Ptr ()))}) v l
msghdrSetcontrol' :: NnMsghdr -> Ptr() -> IO()
msghdrSetcontrol' (NnMsghdr h) l = msghdrSetcontrol'' h l
msghdrSetcontrol ::  NnFMsghdr -> Ptr() -> IO()
msghdrSetcontrol h v = withForeignPtr h (`msghdrSetcontrol''` v)

msghdrGetcontrollen'' = (liftM toInteger) . (\ptr -> do {peekByteOff ptr 24 ::IO CULong})
{-# LINE 381 "System/NanoMsg/C/NanoMsg.chs" #-}
msghdrGetcontrollen' :: NnMsghdr -> IO(Integer)
msghdrGetcontrollen' (NnMsghdr v) = msghdrGetcontrollen'' v
msghdrGetcontrollen :: NnFMsghdr -> IO(Integer)
msghdrGetcontrollen = (`withForeignPtr` msghdrGetcontrollen'')
msghdrSetcontrollen'' v l = (\ptr val -> do {pokeByteOff ptr 24 (val::CULong)}) v (fromInteger l)
msghdrSetcontrollen' :: NnMsghdr -> Integer -> IO()
msghdrSetcontrollen' (NnMsghdr h) l = msghdrSetcontrollen'' h l
msghdrSetcontrollen ::  NnFMsghdr -> Integer -> IO()
msghdrSetcontrollen h v = withForeignPtr h (`msghdrSetcontrollen''` v)

cmsghdrGetlen'' = (liftM toInteger) . (\ptr -> do {peekByteOff ptr 0 ::IO CULong})
{-# LINE 392 "System/NanoMsg/C/NanoMsg.chs" #-}
cmsghdrGetlen' :: NnMsghdr -> IO(Integer)
cmsghdrGetlen' (NnMsghdr v) = cmsghdrGetlen'' v
cmsghdrGetlen :: NnFMsghdr -> IO(Integer)
cmsghdrGetlen = (`withForeignPtr` cmsghdrGetlen'')
cmsghdrSetlen'' v l = (\ptr val -> do {pokeByteOff ptr 0 (val::CULong)}) v (fromInteger l)
cmsghdrSetlen' :: NnMsghdr -> Integer -> IO()
cmsghdrSetlen' (NnMsghdr h) l = cmsghdrSetlen'' h l
cmsghdrSetlen ::  NnFMsghdr -> Integer -> IO()
cmsghdrSetlen h v = withForeignPtr h (`cmsghdrSetlen''` v)


cmsghdrGetlevel'' = (liftM fromIntegral) . (\ptr -> do {peekByteOff ptr 8 ::IO CInt})
{-# LINE 404 "System/NanoMsg/C/NanoMsg.chs" #-}
cmsghdrGetlevel' :: NnMsghdr -> IO(Int)
cmsghdrGetlevel' (NnMsghdr v) = cmsghdrGetlevel'' v
cmsghdrGetlevel :: NnFMsghdr -> IO(Int)
cmsghdrGetlevel = (`withForeignPtr` cmsghdrGetlevel'')
cmsghdrSetlevel'' v l = (\ptr val -> do {pokeByteOff ptr 8 (val::CInt)}) v (fromIntegral l)
cmsghdrSetlevel' :: NnMsghdr -> Int -> IO()
cmsghdrSetlevel' (NnMsghdr h) l = cmsghdrSetlevel'' h l
cmsghdrSetlevel ::  NnFMsghdr -> Int -> IO()
cmsghdrSetlevel h v = withForeignPtr h (`cmsghdrSetlevel''` v)

cmsghdrGettype'' = (liftM fromIntegral) . (\ptr -> do {peekByteOff ptr 12 ::IO CInt})
{-# LINE 415 "System/NanoMsg/C/NanoMsg.chs" #-}
cmsghdrGettype' :: NnMsghdr -> IO(Int)
cmsghdrGettype' (NnMsghdr v) = cmsghdrGettype'' v
cmsghdrGettype :: NnFMsghdr -> IO(Int)
cmsghdrGettype = (`withForeignPtr` cmsghdrGettype'')
cmsghdrSettype'' v l = (\ptr val -> do {pokeByteOff ptr 12 (val::CInt)}) v (fromIntegral l)
cmsghdrSettype' :: NnMsghdr -> Int -> IO()
cmsghdrSettype' (NnMsghdr h) l = cmsghdrSettype'' h l
cmsghdrSettype ::  NnFMsghdr -> Int -> IO()
cmsghdrSettype h v = withForeignPtr h (`cmsghdrSettype''` v)


-- TODO add align
newNnIovec = mallocBytes $ sizeOf (undefined::Ptr a) + 8
{-# LINE 428 "System/NanoMsg/C/NanoMsg.chs" #-}
newNnMsghdr = mallocBytes $ 2 * sizeOf (undefined::Ptr a) + sizeOf (undefined::CInt) + 8
{-# LINE 429 "System/NanoMsg/C/NanoMsg.chs" #-}
newNnCmsghdr = mallocBytes $ 2 * sizeOf (undefined::CInt) + 8
{-# LINE 430 "System/NanoMsg/C/NanoMsg.chs" #-}
newFNnIovec = mallocForeignPtrBytes $ sizeOf (undefined::Ptr a) + 8
{-# LINE 431 "System/NanoMsg/C/NanoMsg.chs" #-}
newFNnMsghdr = mallocForeignPtrBytes $ 2 * sizeOf (undefined::Ptr a) + sizeOf (undefined::CInt) + 8
{-# LINE 432 "System/NanoMsg/C/NanoMsg.chs" #-}
newFNnCmsghdr = mallocForeignPtrBytes $ 2 * sizeOf (undefined::CInt) + 8
{-# LINE 433 "System/NanoMsg/C/NanoMsg.chs" #-}




foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_errno"
  nnErrno'_ :: (IO CInt)

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_symbol"
  nnSymbol'_ :: (CInt -> ((Ptr CInt) -> (IO (Ptr CChar))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_strerror"
  nnStrerror'_ :: (CInt -> (IO (Ptr CChar)))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_term"
  nnTerm :: (IO ())

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_allocmsg"
  nnAllocmsg''_ :: (CULong -> (CInt -> (IO (Ptr ()))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_allocmsg"
  nnAllocmsg'_ :: (CULong -> (CInt -> (IO (Ptr ()))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_freemsg"
  nnFreemsg'_ :: ((Ptr ()) -> (IO CInt))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h wfirsthdr"
  cmsgFirsthdr'_ :: ((Ptr ()) -> (IO (Ptr ())))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h wnxthdr"
  cmsgNxthdr'_ :: ((Ptr ()) -> ((Ptr ()) -> (IO (Ptr ()))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h wdata"
  cmsgData'_ :: ((Ptr ()) -> (IO (Ptr CUChar)))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h wdata"
  cmsgData''_ :: ((Ptr ()) -> (IO (Ptr CUChar)))

foreign import ccall safe "System/NanoMsg/C/NanoMsg.chs.h wlen"
  cmsgLen'_ :: (CULong -> CULong)

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h wlen"
  cmsgLen''_ :: (CULong -> (IO CULong))

foreign import ccall safe "System/NanoMsg/C/NanoMsg.chs.h wspace"
  cmsgSpace'_ :: (CULong -> CULong)

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h wspace"
  cmsgSpace''_ :: (CULong -> (IO CULong))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_socket"
  nnSocket'_ :: (CInt -> (CInt -> (IO CInt)))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_close"
  nnClose'_ :: (CInt -> (IO CInt))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_setsockopt"
  nnSetsockopt'_ :: (CInt -> (CInt -> (CInt -> ((Ptr ()) -> (CULong -> (IO CInt))))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_getsockopt"
  nnGetsockopt'_ :: (CInt -> (CInt -> (CInt -> ((Ptr ()) -> ((Ptr CULong) -> (IO CInt))))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_bind"
  nnBind'_ :: (CInt -> ((Ptr CChar) -> (IO CInt)))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_connect"
  nnConnect'_ :: (CInt -> ((Ptr CChar) -> (IO CInt)))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_shutdown"
  nnShutdown'_ :: (CInt -> (CInt -> (IO CInt)))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_send"
  nnSend'_ :: (CInt -> ((Ptr ()) -> (CULong -> (CInt -> (IO CInt)))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_send"
  nnSend''_ :: (CInt -> ((Ptr ()) -> (CULong -> (CInt -> (IO CInt)))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_send"
  nnSendDyn'_ :: (CInt -> ((Ptr ()) -> (CULong -> (CInt -> (IO CInt)))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_recv"
  nnRecvDyn''_ :: (CInt -> ((Ptr ()) -> (CULong -> (CInt -> (IO CInt)))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_recv"
  nnRecvDyn'_ :: (CInt -> ((Ptr ()) -> (CULong -> (CInt -> (IO CInt)))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_recv"
  nnRecv'_ :: (CInt -> ((Ptr ()) -> (CULong -> (CInt -> (IO CInt)))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_recv"
  nnRecv''_ :: (CInt -> ((Ptr ()) -> (CULong -> (CInt -> (IO CInt)))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_sendmsg"
  nnSendmsg'_ :: (CInt -> ((Ptr ()) -> (CInt -> (IO CInt))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_sendmsg"
  nnSendmsg''_ :: (CInt -> ((Ptr ()) -> (CInt -> (IO CInt))))
