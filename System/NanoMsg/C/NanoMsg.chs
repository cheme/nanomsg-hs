{-# LANGUAGE CPP, ForeignFunctionInterface #-}
  
-- | This module aims at exposing nanomsg function directly to haskell, no api construct except the use of ForeignFreePointers. Function specific documentation is therefore the official nanomsg documentation. An exception is also done for error handling, here we use maybe or either. Note that it is a c2hs module and that returning type of function may contain function parameter when they may be changed (a tuple with firstly function return type then all parameters in order).
-- Send and receive function are not set unsafe, this is thread unsafe, but waiting for some issue (during tests) to set them safe and use similar work arround as in zmq binding (nowait all the time but use haskell threadWriteRead to wait in a ghc non blocking thread way).
module System.NanoMsg.C.NanoMsg where
import System.NanoMsg.C.NanoMsgStruct
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad((<=<),liftM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as U
import qualified Data.List as L
import System.Posix.Types(Fd)
import Control.Concurrent(threadWaitWrite,threadWaitRead)
import qualified Data.ByteString as BS
import Data.List(foldl')
import Control.Monad(foldM,foldM_)
import Data.ByteString.Internal(ByteString(PS))

#include "nanomsg/nn.h"
#include "nanomsg/pair.h"
#include "nanomsg/reqrep.h"
#include "nanomsg/pubsub.h"
#include "nanomsg/survey.h"
#include "nanomsg/pipeline.h"
#include "nanomsg/bus.h"
#include "nanomsg/tcp.h"
#include "nanomsg/inproc.h"
#include "nanomsg/ipc.h"
#include "inlinemacro.h"

-- #include "nanomsg/transport.h"
-- #include "nanomsg/protocol.h"

newtype NnSocket = NnSocket CInt deriving (Eq, Show)

socketToCInt :: NnSocket -> CInt
socketToCInt (NnSocket s) = s

newtype NnEndPoint = NnEndPoint CInt deriving (Eq, Show)

endPointToCInt :: NnEndPoint -> CInt
endPointToCInt (NnEndPoint s) = s

{#enum define NnError 
  { ENOTSUP as ENOTSUP
  , ENOMEM as ENOMEM
  , EPROTONOSUPPORT as EPROTONOSUPPORT
  , ENAMETOOLONG as ENAMETOOLONG
  , ENODEV as ENODEV
  , ENOBUFS as ENOBUFS 
  , ENETDOWN as ENETDOWN 
  , EADDRINUSE as EADDRINUSE 
  , EADDRNOTAVAIL as EADDRNOTAVAIL 
  , ECONNREFUSED as ECONNREFUSED 
  , EINPROGRESS as EINPROGRESS 
  , ENOTSOCK as ENOTSOCK 
  , EAFNOSUPPORT as EAFNOSUPPORT 
  , EPROTO as EPROTO  
  , EAGAIN as EAGAIN 
  , EBADF as EBADF 
  , EINVAL as EINVAL
  , EINTR as EINTR
  , EMFILE as EMFILE 
  , EFAULT as EFAULT  
  , EACCESS as EACCESS  
  , ENETRESET as ENETRESET  
  , ENETUNREACH as ENETUNREACH 
  , EHOSTUNREACH as EHOSTUNREACH 
  , ENOTCONN as ENOTCONN 
  , EMSGSIZE as EMSGSIZE
  , ETIMEDOUT as ETIMEDOUT 
  , ECONNABORTED as ECONNABORTED 
  , ECONNRESET as ECONNRESET 
  , ENOPROTOOPT as ENOPROTOOPT 
  , EISCONN as EISCONN 
  -- natives
  , ETERM as ETERM
  , EFSM as EFSM  } deriving (Eq,Ord,Show) #} -- TODO instance show with nnStrerror (but with code to be able to reverse) 

{#enum define AddressFamilies
  { AF_SP as AF_SP
  , AF_SP_RAW as AF_SP_RAW } deriving (Eq,Ord,Show) #}

{#enum define NnTransport
  { NN_IPC as NN_IPC
  , NN_INPROC as NN_INPROC
  , NN_TCP as NN_TCP } deriving (Eq,Ord,Show) #}
-- TODO link with their protocol
{#enum define ProtocolFamilies
  { NN_PROTO_PUBSUB as NN_PROTO_PUBSUB
  , NN_PROTO_BUS as NN_PROTO_BUS
  , NN_PROTO_PAIR as NN_PROTO_PAIR
  , NN_PROTO_PIPELINE as NN_PROTO_PIPELINE
  , NN_PROTO_REQREP as NN_PROTO_REQREP
  , NN_PROTO_SURVEY as NN_PROTO_SURVEY } deriving (Eq,Ord,Show) #}

{#enum define NnProtocol
  { NN_PUB as NN_PUB
  , NN_SUB as NN_SUB
  , NN_BUS as NN_BUS
  , NN_PAIR as NN_PAIR
  , NN_PUSH as NN_PUSH
  , NN_PULL as NN_PULL
  , NN_REQ as NN_REQ
  , NN_REP as NN_REP
  , NN_SURVEYOR as NN_SURVEYOR
  , NN_RESPONDENT as NN_RESPONDENT } deriving (Eq,Ord,Show) #}


-- | those constants might be useless, if they are of any use we might consider using c2hs #const instead
{#enum define NNConstants
  { NN_SOCKADDR_MAX as NN_SOCKADDR_MAX
  } deriving (Eq,Ord,Show) #}

{#enum define SolSocket
  { NN_SOL_SOCKET as NN_SOL_SOCKET 
  } deriving (Eq,Ord,Show) #}


{#enum define SocketOptions
  { NN_LINGER as NN_LINGER
  , NN_SNDBUF as NN_SNDBUF
  , NN_RCVBUF as NN_RCVBUF
  , NN_SNDTIMEO as NN_SNDTIMEO
  , NN_RCVTIMEO as NN_RCVTIMEO
  , NN_RECONNECT_IVL as NN_RECONNECT_IVL
  , NN_RECONNECT_IVL_MAX as NN_RECONNECT_IVL_MAX
  , NN_SNDPRIO as NN_SNDPRIO
  , NN_SNDFD as NN_SNDFD
  , NN_RCVFD as NN_RCVFD
  , NN_IPV4ONLY as NN_IPV4ONLY  
  } deriving (Eq,Ord,Show) #}

{#enum define SocketReadOptions
  { NN_DOMAIN as NN_DOMAIN
  , NN_PROTOCOL as NN_PROTOCOL
  } deriving (Eq,Ord,Show) #}
 
{#enum define PubSubOptions
  { NN_REQ_RESEND_IVL as NN_REQ_RESEND_IVL
  } deriving (Eq,Ord,Show) #}

{#enum define SurveyOptions
  { NN_SURVEYOR_DEADLINE as NN_SURVEYOR_DEADLINE
  } deriving (Eq,Ord,Show) #}


{#enum define ReqRepOptions
  { NN_SUB_SUBSCRIBE as NN_SUB_SUBSCRIBE
  , NN_SUB_UNSUBSCRIBE as NN_SUB_UNSUBSCRIBE
  } deriving (Eq,Ord,Show) #}

{#enum define TcpOptions
  { NN_TCP_NODELAY as NN_TCP_NODELAY
  } deriving (Eq,Ord,Show) #}


-- | internal only
class (Enum a, Show a, Eq a) => AllSocketOptions a
instance AllSocketOptions ReqRepOptions
instance AllSocketOptions SurveyOptions
instance AllSocketOptions PubSubOptions
instance AllSocketOptions SocketOptions
instance AllSocketOptions TcpOptions
instance AllSocketOptions SocketReadOptions
-- | internal only
class (Enum a, Show a, Eq a) => AllLevelOptions a
instance AllLevelOptions NnProtocol
instance AllLevelOptions SolSocket
instance AllLevelOptions NnTransport




{#enum define SndRcvFlags
  { NN_DONTWAIT as NN_DONTWAIT } deriving (Eq,Ord,Show) #}

flagsToCInt :: Enum a => [a] -> CInt
flagsToCInt b = L.foldl' (\ac en -> ac + cIntFromEnum en ) 0  b

rFlagsToCInt :: Enum a => [a] -> (CInt,Bool)
rFlagsToCInt b = let f = L.foldl' (\ac en -> ac + cIntFromEnum en ) 0  b in
                 if (nN_DONTWAIT .&. f) == 0 then (nN_DONTWAIT `xor` f, True) else (f, False)

sFlagsToCInt :: Enum a => [a] -> (CInt,Bool)
sFlagsToCInt = rFlagsToCInt

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = fromIntegral . fromEnum

peekInt :: (Storable a, Integral a) => Ptr a -> IO Int
peekInt = (liftM fromIntegral) . peek

-- TODO inline
withPStorable :: (Storable a) => a -> (Ptr a -> IO b)  -> IO b
withPStorable i r = alloca (\p -> poke p i >> r p) 

withPStorable' :: (Storable a) => a -> (Ptr c -> IO b)  -> IO b
withPStorable' i r = alloca (\p -> poke p i >> r (castPtr p)) 

withPIntegral :: (Storable a, Num a, Integral c) => c -> (Ptr a -> IO b)  -> IO b
withPIntegral i = withPStorable (fromIntegral i)

--withPForeign :: ForeignPtr () -> (Ptr () -> IO b)  -> IO b
--withPForeign fp r = withForeignPtr fp (\p -> alloca (\pp -> poke pp p >> r (castPtr pp)))

withPPtr :: Ptr () -> (Ptr () -> IO b)  -> IO b
withPPtr p r = alloca (\pp -> poke pp p >> r (castPtr pp))


foreignFree :: Ptr a -> IO(ForeignPtr a)
foreignFree = newForeignPtr finalizerFree

foreignVoid :: Ptr () -> IO(ForeignPtr ())
foreignVoid = newForeignPtr finalizerFree

foreignPMsg :: Ptr () -> IO(ForeignPtr ())
foreignPMsg pv = do 
  v <- peek (castPtr pv)
  free pv
  newForeignPtr nnFunPtrFreeMsg v

pVoid :: Ptr () -> IO(Ptr ())
pVoid pv = do 
  v <- peek (castPtr pv)
  free pv
  return v


foreignFreeMsg :: Ptr () -> IO(Either NnError (ForeignPtr ()))
foreignFreeMsg =  either (return . Left) (return . Right <=< newForeignPtr nnFunPtrFreeMsg) <=< errorFromNewPointer

cPackCString :: CString -> IO ByteString
cPackCString = C.packCString
ucPackCString :: Ptr a -> IO ByteString
ucPackCString = C.packCString . castPtr
uPackCString :: CString -> IO ByteString
uPackCString = U.unsafePackCString
uuPackCString :: Ptr a -> IO ByteString
uuPackCString = U.unsafePackCString . castPtr

errorFromRetCode :: CInt -> IO(Maybe NnError)
errorFromRetCode r = if r < 0 then nnErrno >>= return . Just else return Nothing

errorFromNewPointer :: Ptr () -> IO(Either NnError (Ptr ()))
errorFromNewPointer ptr = if ptr == nullPtr then nnErrno >>= return . Left else return $ Right ptr


errorFromLength :: CInt -> IO(Either NnError Int)
errorFromLength r = if r < 0 then nnErrno >>= return . Left else (return . Right . fromIntegral) r

errorFromSocket :: CInt -> IO(Either NnError NnSocket)
errorFromSocket r = if r < 0 then nnErrno >>= return . Left else (return . Right . NnSocket) r

errorFromEndPoint :: CInt -> IO(Either NnError NnEndPoint)
errorFromEndPoint r = if r < 0 then nnErrno >>= return . Left else (return . Right . NnEndPoint) r


{#fun unsafe nn_errno  as ^ {} -> `NnError' cIntToEnum #}
-- Mostly useless as c2hs uses macro, so no mapping from Int values to Enum
{#fun unsafe nn_symbol as ^ {fromIntegral `Int', alloca- `Int' peekInt*} -> `String' peekCString* #}
{#fun unsafe nn_strerror as ^ {cIntFromEnum `NnError' } -> `String' peekCString* #}

dummy' :: IO ()
dummy' = {#call unsafe nn_term as ^ #}

-- type of allocation is transport dependant -- see transport implementation --> for haskell api link it to the transport used -- TODO (tricky??)
{#fun unsafe nn_allocmsg as nnAllocmsg' { fromIntegral `Int', fromIntegral `Int'} -> `Either NnError (ForeignPtr ())' foreignFreeMsg* #}
{#fun unsafe nn_allocmsg as ^ { fromIntegral `Int', fromIntegral `Int'} -> `Either NnError (Ptr ())' errorFromNewPointer* #}

-- do not use if nnAllocmsg' used
{#fun unsafe nn_freemsg as ^ {id `Ptr ()'} -> `Maybe NnError' errorFromRetCode* #}

{#fun unsafe wfirsthdr as cmsgFirstHdr {fromMsgHdr* `NNMsgHdr'} -> `Maybe NNCMsgHdr' maybeCMsg*#}
{#fun unsafe wnxthdr as cmsgNxtHdr {fromMsgHdr* `NNMsgHdr', fromCMsgHdr* `NNCMsgHdr'} -> `Maybe NNCMsgHdr' maybeCMsg* #}
-- | use of byteString for char * here. Note that Bytestring is copied. -- not we use unsigned char and do a cast ptr : ByteString should not be use with unicode.
{#fun unsafe wdata as cmsgData {fromCMsgHdr* `NNCMsgHdr'} -> `ByteString' ucPackCString* #}
-- | unsafe version for efficiency. To test but might be ok.
{#fun unsafe wdata as cmsgData' {fromCMsgHdr* `NNCMsgHdr'} -> `ByteString' uuPackCString* #}
-- | might not be pure in the future but given current nanomsg implementation it is ok
{#fun pure wlen as cmsgLen { fromIntegral `Int'} -> `Int' fromIntegral #}
{#fun unsafe wlen as cmsgLen' { fromIntegral `Int'} -> `Int' fromIntegral #}
-- | might not be pure in the future but given current nanomsg implementation it is ok
{#fun pure wspace as cmsgSpace { fromIntegral `Int'} -> `Int' fromIntegral #}
{#fun unsafe wspace as cmsgSpace' { fromIntegral `Int'} -> `Int' fromIntegral #}


-- TODO enum for domain???
{#fun unsafe nn_socket as ^ { cIntFromEnum `AddressFamilies', cIntFromEnum `NnProtocol'} -> `Either NnError NnSocket' errorFromSocket* #}

{#fun unsafe nn_close as ^ {socketToCInt `NnSocket'} -> `Maybe NnError' errorFromRetCode* #}

{#fun unsafe nn_setsockopt as ^ `(AllSocketOptions a, AllLevelOptions b)' => {socketToCInt `NnSocket', cIntFromEnum `b', cIntFromEnum `a', id `Ptr ()', fromIntegral `Int'} -> `Maybe NnError' errorFromRetCode* #}

withNullPPtr :: (Ptr () -> IO b) -> IO b
withNullPPtr r = do 
  pp <- malloc
  poke pp nullPtr
  r $ castPtr pp
withNullPtr :: (Ptr () -> IO b) -> IO b
withNullPtr r = r nullPtr
-- | handling of values for options out of c api - we do not allocate memory for return value of option -- and do not send size -- should use a stablepointer?? -- to test thoroughly (doc initiate size and pointer which does not make any sense
--{#fun unsafe nn_getsockopt as ^ `(AllSocketOptions a, AllLevelOptions b)' => {socketToCInt `NnSocket', cIntFromEnum `b', cIntFromEnum `a', withNullPtr- `Ptr ()' id,  alloca- `Int' peekInt*} -> `Maybe NnError' errorFromRetCode* #}
{#fun unsafe nn_getsockopt as ^ `(AllSocketOptions a, AllLevelOptions b)' => {socketToCInt `NnSocket', cIntFromEnum `b', cIntFromEnum `a', id `Ptr ()' id, withPIntegral* `Int' peekInt*} -> `Maybe NnError' errorFromRetCode* #}

-- TODO bind an address type to avoid address without :// (in api(one per transport) using NN_SOCKADDR_MAX)
{#fun unsafe nn_bind as ^ {socketToCInt `NnSocket', withCString* `String'} ->  `Either NnError NnEndPoint' errorFromEndPoint* #}

{#fun unsafe nn_connect as ^ {socketToCInt `NnSocket', withCString* `String'} ->  `Either NnError NnEndPoint' errorFromEndPoint* #}

{#fun unsafe nn_shutdown as ^ {socketToCInt `NnSocket', endPointToCInt `NnEndPoint'} -> `Maybe NnError' errorFromRetCode* #}

-- All recv functions are derived from C2hs generation to enforce nn_dont_wait and depending on set flags wait in haskell (issue with poll c function when using ffi). Normal c2hs receive are kept postfixed B (as Bogus).
{#fun unsafe nn_recv as nnRecvDynB' {socketToCInt `NnSocket', withNullPPtr- `Ptr ()' pVoid*,  withNnMSG- `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
{#fun unsafe nn_recv as nnRecvDynB {socketToCInt `NnSocket', withNullPPtr- `ForeignPtr ()' foreignPMsg*,  withNnMSG- `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
-- TODO fn with foreign does not make too much sense (should be in api)
{#fun unsafe nn_recv as nnRecvB {socketToCInt `NnSocket', withForeignPtr* `ForeignPtr ()', fromIntegral `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
{#fun unsafe nn_recv as nnRecvB' {socketToCInt `NnSocket', id `Ptr ()', fromIntegral `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}

{#fun unsafe nn_recvmsg as nn_recvmsgB {socketToCInt `NnSocket', fromMsgHdr* `NNMsgHdr', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
{#fun unsafe nn_recvmsg as nnRecvfmsgB {socketToCInt `NnSocket', fromFMsgHdr* `NNFMsgHdr', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_recv"
  nn_recv_c :: (CInt -> ((Ptr ()) -> (CULong -> (CInt -> (IO CInt)))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_recvmsg"
  nn_recvmsg_c :: (CInt -> ((Ptr ()) -> (CInt -> (IO CInt))))

pollRecFd :: Bool -> CInt -> Ptr () -> CULong -> CInt -> IO CInt
pollRecFd True s ptr psize fls = do
  res <- nn_recv_c s ptr psize fls
  if res == (-1) then do
    err <- nnErrno
    if err == EAGAIN then do
      getFd NN_RCVFD s >>= threadWaitRead >> pollRecFd True s ptr psize fls
    else return res
  else return res
pollRecFd False s ptr psize fls = nn_recv_c s ptr psize fls
pollRecFdMsg :: Bool -> CInt -> Ptr () -> CInt -> IO CInt
pollRecFdMsg True s ptr fls = do
  res <- nn_recvmsg_c s ptr fls
  if res == (-1) then do
    err <- nnErrno
    if err == EAGAIN then do
      getFd NN_RCVFD s >>= threadWaitRead >> pollRecFdMsg True s ptr fls 
    else return res
  else return res
pollRecFdMsg False s ptr fls = nn_recvmsg_c s ptr fls

pollSndFd :: Bool -> CInt -> Ptr () -> CULong -> CInt -> IO CInt
pollSndFd True s ptr psize fls = do
  res <- nn_send_c s ptr psize fls
  if res == (-1) then do
    err <- nnErrno
    if err == EAGAIN then do
      getFd NN_SNDFD s >>= threadWaitWrite >> pollSndFd True s ptr psize fls
    else return res
  else return res
pollSndFd False s ptr psize fls = nn_send_c s ptr psize fls

pollSndFdMsg :: Bool -> CInt -> Ptr () -> CInt -> IO CInt
pollSndFdMsg True s ptr fls = do
  res <- nn_sendmsg_c s ptr fls
  if res == (-1) then do
    err <- nnErrno
    if err == EAGAIN then do
      getFd NN_SNDFD s >>= threadWaitWrite >> pollSndFdMsg True s ptr fls
    else return res
  else return res
pollSndFdMsg False s ptr fls = nn_sendmsg_c s ptr fls


-- TODO manage error (no fd result from getOption)
getFd :: (Enum c) => c -> CInt -> IO Fd
getFd f s = 
  let sol = cIntFromEnum NN_SOL_SOCKET
      fdo = cIntFromEnum f 
      fdSize = fromIntegral $ sizeOf (undefined :: Fd) in
    alloca $ \ptr ->
      alloca $ \psize -> do
        poke psize fdSize
        size <- nnGetsockopt'_ s sol fdo (castPtr (ptr :: Ptr Fd)) psize
        peek ptr

-- boileplate copy from c2hs generated code to include test on dont wait and haskell polling of file descriptor
nnRecvDyn' :: (NnSocket) -> ([SndRcvFlags]) -> IO ((Either NnError Int), (Ptr ()))
nnRecvDyn' soc fls =
  let s = socketToCInt soc in 
  withNullPPtr $ \nulptr -> 
  withNnMSG $ \size -> 
  let {(f,w) = rFlagsToCInt fls} in 
  pollRecFd w s nulptr size f >>= \res ->
  errorFromLength res >>= \res' ->
  pVoid  nulptr>>= \a2'' -> 
  return (res', a2'')
nnRecvDyn :: (NnSocket) -> ([SndRcvFlags]) -> IO ((Either NnError Int), (ForeignPtr ()))
nnRecvDyn a1 a4 =
  let {a1' = socketToCInt a1} in 
  withNullPPtr $ \a2' -> 
  withNnMSG $ \a3' -> 
  let {(a4',w) = rFlagsToCInt a4} in 
  pollRecFd w a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  foreignPMsg  a2'>>= \a2'' -> 
  return (res', a2'')
nnRecv :: (NnSocket) -> (ForeignPtr ()) -> (Int) -> ([SndRcvFlags]) -> IO ((Either NnError Int))
nnRecv a1 a2 a3 a4 =
  let {a1' = socketToCInt a1} in 
  withForeignPtr a2 $ \a2' -> 
  let {a3' = fromIntegral a3} in 
  let {(a4',w) = rFlagsToCInt a4} in 
  pollRecFd w a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')

nnRecv' :: (NnSocket) -> (Ptr ()) -> (Int) -> ([SndRcvFlags]) -> IO ((Either NnError Int))
nnRecv' a1 a2 a3 a4 =
  let {a1' = socketToCInt a1} in 
  let {a2' = id a2} in 
  let {a3' = fromIntegral a3} in 
  let {(a4',w) = rFlagsToCInt a4} in 
  pollRecFd w a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')

nnRecvmsg :: (NnSocket) -> (NNMsgHdr) -> ([SndRcvFlags]) -> IO ((Either NnError Int))
nnRecvmsg a1 a2 a3 =
  let {a1' = socketToCInt a1} in 
  fromMsgHdr a2 $ \a2' -> 
  let {(a3',w) = rFlagsToCInt a3} in 
  pollRecFdMsg w a1' a2' a3' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')

nnRecvfmsg :: (NnSocket) -> (NNFMsgHdr) -> ([SndRcvFlags]) -> IO ((Either NnError Int))
nnRecvfmsg a1 a2 a3 =
  let {a1' = socketToCInt a1} in 
  fromFMsgHdr a2 $ \a2' -> 
  let {(a3',w) = rFlagsToCInt a3} in 
  pollRecFdMsg w a1' a2' a3' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')


-- | type to send not in C (not even storable)
{#fun unsafe nn_send as nnSendB {socketToCInt `NnSocket', withForeignPtr* `ForeignPtr ()', fromIntegral `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
-- | not ForeignFree
{#fun unsafe nn_send as nnSendB' {socketToCInt `NnSocket', id `Ptr ()', fromIntegral `Int',flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
-- | no foreign (deallocate is managed by nanomq)
{#fun unsafe nn_send as nnSendDynB {socketToCInt `NnSocket', withPPtr* `Ptr ()', withNnMSG- `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
--{#fun unsafe nn_send as nnSendDyn {socketToCInt `NnSocket', withPForeign* `ForeignPtr ()', withNnMSG- `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #} -- Do no send with foreing free pointer because nn deallocate

{#fun unsafe nn_sendmsg as nnSendmsgB {socketToCInt `NnSocket', fromMsgHdr* `NNMsgHdr', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
{#fun unsafe nn_sendmsg as nnSendfmsgB {socketToCInt `NnSocket', fromFMsgHdr* `NNFMsgHdr', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_send"
  nn_send_c :: (CInt -> ((Ptr ()) -> (CULong -> (CInt -> (IO CInt)))))

foreign import ccall unsafe "System/NanoMsg/C/NanoMsg.chs.h nn_sendmsg"
  nn_sendmsg_c :: (CInt -> ((Ptr ()) -> (CInt -> (IO CInt))))


-- same as with receive (issue with poll)
-- | type to send not in C (not even storable) 
nnSend :: (NnSocket) -> (ForeignPtr ()) -> (Int) -> ([SndRcvFlags]) -> IO ((Either NnError Int))
nnSend a1 a2 a3 a4 =
  let {a1' = socketToCInt a1} in 
  withForeignPtr a2 $ \a2' -> 
  let {a3' = fromIntegral a3} in 
  let {(a4',w) = sFlagsToCInt a4} in 
  pollSndFd w a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')
-- | not ForeignFree
nnSend' :: (NnSocket) -> (Ptr ()) -> (Int) -> ([SndRcvFlags]) -> IO ((Either NnError Int))
nnSend' a1 a2 a3 a4 =
  let {a1' = socketToCInt a1} in 
  let {a2' = id a2} in 
  let {a3' = fromIntegral a3} in 
  let {(a4',w) = sFlagsToCInt a4} in 
  pollSndFd w a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')
{-# LINE 315 "System/NanoMsg/C/NanoMsg.chs" #-}
-- | no foreign (deallocate is managed by nanomq)
nnSendDyn :: (NnSocket) -> (Ptr ()) -> ([SndRcvFlags]) -> IO ((Either NnError Int))
nnSendDyn a1 a2 a4 =
  let {a1' = socketToCInt a1} in 
  withPPtr a2 $ \a2' -> 
  withNnMSG $ \a3' -> 
  let {(a4',w) = sFlagsToCInt a4} in 
  pollSndFd w a1' a2' a3' a4' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')
nnSendmsg :: (NnSocket) -> (NNMsgHdr) -> ([SndRcvFlags]) -> IO ((Either NnError Int))
nnSendmsg a1 a2 a3 =
  let {a1' = socketToCInt a1} in 
  fromMsgHdr a2 $ \a2' -> 
  let {(a3',w) = sFlagsToCInt a3} in 
  pollSndFdMsg w a1' a2' a3' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')
nnSendfmsg :: (NnSocket) -> (NNFMsgHdr) -> ([SndRcvFlags]) -> IO ((Either NnError Int))
nnSendfmsg a1 a2 a3 =
  let {a1' = socketToCInt a1} in 
  fromFMsgHdr a2 $ \a2' -> 
  let {(a3',w) = sFlagsToCInt a3} in 
  pollSndFdMsg w a1' a2' a3' >>= \res ->
  errorFromLength res >>= \res' ->
  return (res')

withFmsghdr :: ForeignPtr a -> (Ptr c -> IO b) -> IO b
withFmsghdr f r =  withForeignPtr f (r . castPtr)

withNnMSG :: Num a => (a -> b) -> b
withNnMSG a = a (fromIntegral nN_MSG)

-- TODO api with fork (with correct mask) + socket type incompatibilities?
{#fun unsafe nn_device as ^ {socketToCInt `NnSocket', socketToCInt `NnSocket'} -> `Maybe NnError' errorFromRetCode* #}

-- Struct related Code for simplicity and to avoid boilerplate code, this could be refactor in a separate hs2c module with usage of data, or moved in c helper functions.

fromMsgHdr ::  NNMsgHdr -> (Ptr () -> IO b) -> IO b
fromMsgHdr  = withPStorable'
fromFMsgHdr ::  NNFMsgHdr -> (Ptr () -> IO b) -> IO b
fromFMsgHdr  = withPStorable'
fromCMsgHdr ::  NNCMsgHdr -> (Ptr () -> IO b) -> IO b
fromCMsgHdr  = withPStorable'
toCMsgHdr :: Ptr () -> IO NNCMsgHdr
toCMsgHdr = peek . castPtr

maybeCMsg :: Ptr () -> IO (Maybe NNCMsgHdr)
maybeCMsg m = if m == nullPtr then return Nothing else toCMsgHdr m >>= (return . Just)

-- | MsgHdr helpers

type MsgHdr = Ptr NNCMsgHdr

newRawMsgHdr :: (Storable s) => s -> IO (Either NnError MsgHdr)
newRawMsgHdr s = do 
  let totle = sizeOf s
  mem <- nnAllocmsg (totle) 0
  case mem of 
    (Left e) -> return $ Left e
    (Right ptr') -> do
      let ptr = castPtr ptr'
      poke ptr s
      return $ Right ptr

getRawMsgHdr :: (Storable s) => MsgHdr -> IO (Either NnError s)
getRawMsgHdr pt = do 
   r  <- peek (castPtr pt)
   er <- nnFreemsg (castPtr pt) -- as bs are copied
   case er of
      (Just e) -> return $ Left e
      Nothing  -> return $ Right r
 
newMSgHdr :: [(BS.ByteString, Int, Int)] -> IO (Either NnError MsgHdr)
newMSgHdr hdrs = do 
  let totle = foldl' (\acc (bs, _, _)-> acc + BS.length bs + hdrsize) 0 hdrs -- TODO do not use BS.length
  mem <- nnAllocmsg (totle) 0
  case mem of 
    (Left e) -> return $ Left e
    (Right ptr') -> do
      let ptr = castPtr ptr'
      foldM_ (pokeEl (ptr)) 0 hdrs
      return $ Right ptr
    where pokeEl :: MsgHdr -> Int -> (BS.ByteString, Int, Int) -> IO Int
          pokeEl ptr offset ((PS pbs pof ple), lev, typ ) = do
                                        poke  (ptr `plusPtr` offset) $ NNCMsgHdr (fromIntegral (ple - pof)) (fromIntegral lev) (fromIntegral typ)
                                        --pokeElemOff ptr (offset + sizeOf hdr) bs
                                        withForeignPtr pbs $ \pbs' -> copyBytes (ptr `plusPtr` (offset + hdrsize)) ((castPtr pbs') `plusPtr` pof) (ple - pof) -- TODO use unsafe bytestring api instead
                                        return (offset + hdrsize + (ple - pof))
          hdrsize = sizeOf (undefined :: NNCMsgHdr)

-- test MsgHdr not nullPtr not here
getMSgHdr :: MsgHdr -> Int -> IO (Either NnError [(BS.ByteString, Int, Int)])
getMSgHdr pt nbh = do
  r <- if nbh == fromIntegral nN_MSG then do
     id <- peek (castPtr pt) :: IO CUInt
     return [(BS.pack [],fromIntegral id,0)]
  else do 
     (_,r) <- foldM (\acc _ -> peekmsg pt acc ) (0,[]) [1..nbh]
     return r
  er <- nnFreemsg (castPtr pt) -- as bs are copied
  case er of
      (Just e) -> return $ Left e
      Nothing  -> return $ Right $ reverse r
  where
         peekmsg ptr' (offset, acc) = do 
           let ptr =  ptr' `plusPtr` offset
           ptch <- peek ptr
           bs <- BS.packCStringLen (ptr `plusPtr` (sizeOf ptch ), (fromIntegral (cmsglen ptch))) -- here use bytestring PS instead
           let newoffset = offset + sizeOf ptch + fromIntegral (cmsglen ptch)
           return (newoffset, (bs, fromIntegral (cmsglev ptch), fromIntegral (cmsgtyp ptch)) : acc)



