{-# LANGUAGE CPP, ForeignFunctionInterface #-}
  
module System.NanoMsg.C.NanoMsg where
-- | This module aims at exposing nanomsg function directly to haskell, no api construct except the use of ForeignFreePointers. Function specific documentation is therefore the official nanomsg documentation. An exception is also done for error handling, here we use maybe or either. Note that it is a c2hs module and that returning type of function may contain function parameter when they may be changed (a tuple with firstly function return type then all parameters in order).
-- Send and receive function are not set unsafe, this is thread unsafe, but waiting for some issue (during tests) to set them safe and use similar work arround as in zmq binding (nowait all the time but use haskell threadWriteRead to wait in a ghc non blocking thread way).
import System.NanoMsg.C.NanoMsgStruct
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
import qualified Data.List as L

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

-- TODO change all size_t to Int (currently Int)

-- #include "nanomsg/transport.h"
-- #include "nanomsg/protocol.h"
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
-- TODO associate to their protocol
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




{#enum define SndRcvFlags
  { NN_DONTWAIT as NN_DONTWAIT } deriving (Eq,Ord,Show) #}

flagsToCInt :: Enum a => [a] -> CInt
flagsToCInt b = L.foldl' (\ac en -> ac + cIntFromEnum en ) 0  b

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = fromIntegral . fromEnum

peekInt :: (Storable a, Integral a) => Ptr a -> IO Int
peekInt = (liftM fromIntegral) . peek

-- TODO inline
withPStorable :: (Storable a) => a -> (Ptr a -> IO b)  -> IO b
withPStorable i r = alloca (\p -> poke p i >> r p) 
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

cPackCString = C.packCString
ucPackCString = C.packCString . castPtr
uPackCString = U.unsafePackCString
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

-- TODO code from sample to catch errors
dummy' = {#call unsafe nn_term as ^ #}

-- type of allocation is transport dependant -- see transport implementation --> for haskell api link it to the transport used -- TODO (tricky??)
{#fun unsafe nn_allocmsg as nnAllocmsg' { fromIntegral `Int', fromIntegral `Int'} -> `Either NnError (ForeignPtr ())' foreignFreeMsg* #}
{#fun unsafe nn_allocmsg as ^ { fromIntegral `Int', fromIntegral `Int'} -> `Either NnError (Ptr ())' errorFromNewPointer* #}

-- do not use if nnAllocmsg' used
{#fun unsafe nn_freemsg as ^ {id `Ptr ()'} -> `Maybe NnError' errorFromRetCode* #}

-- not c2hs (to type it explicitly as a funPtr for finalizer) TODO test if discarding result is an issue with linking -- Internal use only 
foreign import ccall "System/NanoMsg/C/NanoMsg.chs.h &nn_freemsg"
   nnFunPtrFreeMsg :: FunPtr (Ptr () -> IO ())

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


newtype NnSocket = NnSocket CInt deriving (Eq, Show)
socketToCInt (NnSocket s) = s
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

newtype NnEndPoint = NnEndPoint CInt deriving (Eq, Show)
endPointToCInt (NnEndPoint s) = s
-- TODO bind an address type to avoid address without :// (in api(one per transport) using NN_SOCKADDR_MAX)
{#fun unsafe nn_bind as ^ {socketToCInt `NnSocket', withCString* `String'} ->  `Either NnError NnEndPoint' errorFromEndPoint* #}

{#fun unsafe nn_connect as ^ {socketToCInt `NnSocket', withCString* `String'} ->  `Either NnError NnEndPoint' errorFromEndPoint* #}

{#fun unsafe nn_shutdown as ^ {socketToCInt `NnSocket', endPointToCInt `NnEndPoint'} -> `Maybe NnError' errorFromRetCode* #}

-- | type to send not in C (not even storable) 
{#fun  nn_send as ^ {socketToCInt `NnSocket', withForeignPtr* `ForeignPtr ()', fromIntegral `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
-- | not ForeignFree
{#fun  nn_send as nnSend' {socketToCInt `NnSocket', id `Ptr ()', fromIntegral `Int',flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
-- | no foreign (deallocate is managed by nanomq)
{#fun  nn_send as nnSendDyn {socketToCInt `NnSocket', withPPtr* `Ptr ()', withNnMSG- `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
--{#fun unsafe nn_send as nnSendDyn {socketToCInt `NnSocket', withPForeign* `ForeignPtr ()', withNnMSG- `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #} -- Do no send with foreing free pointer because nn deallocate

-- TODO fn with foreign does not make too much sense (should be in api)
{#fun nn_recv as nnRecvDyn' {socketToCInt `NnSocket', withNullPPtr- `Ptr ()' pVoid*,  withNnMSG- `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
{#fun  nn_recv as nnRecvDyn {socketToCInt `NnSocket', withNullPPtr- `ForeignPtr ()' foreignPMsg*,  withNnMSG- `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}

-- TODO fn with foreign does not make too much sense (should be in api)
{#fun  nn_recv as ^ {socketToCInt `NnSocket', withForeignPtr* `ForeignPtr ()', fromIntegral `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
{#fun  nn_recv as nnRecv' {socketToCInt `NnSocket', id `Ptr ()', fromIntegral `Int', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}


{#fun unsafe nn_sendmsg as ^ {socketToCInt `NnSocket', fromMsgHdr* `NNMsgHdr', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}

{#fun nn_recvmsg as ^ {socketToCInt `NnSocket', fromMsgHdr* `NNMsgHdr', flagsToCInt `[SndRcvFlags]'} -> `Either NnError Int' errorFromLength* #}
--withFmsghdr :: NnFMsghdr -> Ptr()
withFmsghdr f r =  withForeignPtr f (r . castPtr)
-- | Warning value of constant NN_MSG is hardcoded to (-1). Due to restriction on cast with c2hs. TODO use #const in a separate hs2c file or use inline macro to cast to an int (dirty).
withNnMSG a = a (fromIntegral nN_MSG)

-- | TODO api with fork (with correct mask) + socket type incompatibilities?
{#fun unsafe nn_device as ^ {socketToCInt `NnSocket', socketToCInt `NnSocket'} -> `Maybe NnError' errorFromRetCode* #}
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
fromMsgHdr ::  NNMsgHdr -> (Ptr () -> IO b)  -> IO b
fromMsgHdr  = withPStorable'
fromCMsgHdr ::  NNCMsgHdr -> (Ptr () -> IO b)  -> IO b
fromCMsgHdr  = withPStorable'
toCMsgHdr :: Ptr () -> IO NNCMsgHdr
toCMsgHdr = peek . castPtr
maybeCMsg m = if m == nullPtr then return Nothing else toCMsgHdr m >>= (return . Just)


