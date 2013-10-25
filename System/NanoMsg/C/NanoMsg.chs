{-# LANGUAGE CPP, ForeignFunctionInterface #-}

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


{#fun unsafe nn_errno  as ^ {} -> `NnError' cIntToEnum #}
-- Mostly useless as c2hs uses macro, so no mapping from Int values to Enum
{#fun unsafe nn_symbol as ^ {fromIntegral `Int', alloca- `Int' peekInt*} -> `String' peekCString* #}
{#fun unsafe nn_strerror as ^ {cIntFromEnum `NnError' } -> `String' peekCString* #}

-- TODO code from sample to catch errors
dummy' = {#call unsafe nn_term as ^ #}

-- type of allocation is transport dependant -- see transport implementation --> for haskell api link it to the transport used -- TODO (tricky??)
{#fun unsafe nn_allocmsg as nnAllocmsg' { fromIntegral `Integer', fromIntegral `Int'} -> `Either NnError (ForeignPtr ())' foreignFreeMsg* #}
{#fun unsafe nn_allocmsg as ^ { fromIntegral `Integer', fromIntegral `Int'} -> `Either NnError (Ptr ())' errorFromNewPointer* #}

-- do not use if nnAllocmsg' used
{#fun unsafe nn_freemsg as ^ {id `Ptr ()'} -> `Maybe NnError' errorFromRetCode* #}

-- not c2hs (to type it explicitly as a funPtr for finalizer) TODO test if discarding result is an issue with linking -- Internal use only 
foreign import ccall "System/NanoMsg/C/NanoMsg.chs.h &nn_freemsg"
   nnFunPtrFreeMsg :: FunPtr (Ptr () -> IO ())

{#fun unsafe wfirsthdr as cmsgFirsthdr {fromMsghdr `NnMsghdr'} -> `NnCmsghdr' toCmsghdr#}
{#fun unsafe wnxthdr as cmsgNxthdr {fromMsghdr `NnMsghdr', fromCmsghdr `NnCmsghdr'} -> `Maybe NnCmsghdr' maybeCmsg #}
-- | use of byteString for char * here. Note that Bytestring is copied. -- not we use unsigned char and do a cast ptr : ByteString should not be use with unicode.
{#fun unsafe wdata as cmsgData {fromCmsghdr `NnCmsghdr'} -> `ByteString' ucPackCString* #}
-- | unsafe version for efficiency. To test but might be ok.
{#fun unsafe wdata as cmsgData' {fromCmsghdr `NnCmsghdr'} -> `ByteString' uuPackCString* #}
-- | might not be pure in the future but given current nanomsg implementation it is ok
{#fun pure wlen as cmsgLen { fromIntegral `Integer'} -> `Integer' toInteger #}
{#fun unsafe wlen as cmsgLen' { fromIntegral `Integer'} -> `Integer' toInteger #}
-- | might not be pure in the future but given current nanomsg implementation it is ok
{#fun pure wspace as cmsgSpace { fromIntegral `Integer'} -> `Integer' toInteger #}
{#fun unsafe wspace as cmsgSpace' { fromIntegral `Integer'} -> `Integer' toInteger #}


newtype NnSocket = NnSocket CInt deriving (Eq, Show)
socketToCInt (NnSocket s) = s
-- TODO enum for domain???
{#fun unsafe nn_socket as ^ { cIntFromEnum `AddressFamilies', cIntFromEnum `NnProtocol'} -> `Either NnError NnSocket' errorFromSocket* #}

{#fun unsafe nn_close as ^ {socketToCInt `NnSocket'} -> `Maybe NnError' errorFromRetCode* #}

{#fun unsafe nn_setsockopt as ^ `(AllSocketOptions a, AllLevelOptions b)' => {socketToCInt `NnSocket', cIntFromEnum `b', cIntFromEnum `a', id `Ptr ()', fromIntegral `Integer'} -> `Maybe NnError' errorFromRetCode* #}

withNullPtr :: (Ptr () -> IO b) -> IO b
withNullPtr r = r nullPtr
-- | handling of values for options out of c api - we do not allocate memory for return value of option -- and do not send size -- should use a stablepointer?? -- to test thoroughly (doc initiate size and pointer which does not make any sense
{#fun unsafe nn_getsockopt as ^ `(AllSocketOptions a, AllLevelOptions b)' => {socketToCInt `NnSocket', cIntFromEnum `b', cIntFromEnum `a', withNullPtr- `Ptr ()' id,  alloca- `Integer' peekInteger*} -> `Maybe NnError' errorFromRetCode* #}

newtype NnEndPoint = NnEndPoint CInt deriving (Eq, Show)
endPointToCInt (NnEndPoint s) = s
-- TODO bind an address type to avoid address without :// (in api(one per transport) using NN_SOCKADDR_MAX)
{#fun unsafe nn_bind as ^ {socketToCInt `NnSocket', withCString* `String'} ->  `Either NnError NnEndPoint' errorFromEndPoint* #}

{#fun unsafe nn_connect as ^ {socketToCInt `NnSocket', withCString* `String'} ->  `Either NnError NnEndPoint' errorFromEndPoint* #}

{#fun unsafe nn_shutdown as ^ {socketToCInt `NnSocket', endPointToCInt `NnEndPoint'} -> `Maybe NnError' errorFromRetCode* #}

-- | type to send not in C (not even storable)
{#fun unsafe nn_send as ^ {socketToCInt `NnSocket', withForeignPtr* `ForeignPtr ()', fromIntegral `Integer', cIntFromEnum `SndRcvFlags'} -> `Either NnError Integer' errorFromLength* #}
-- | not ForeignFree
{#fun unsafe nn_send as nnSend' {socketToCInt `NnSocket', id `Ptr ()', fromIntegral `Integer', cIntFromEnum `SndRcvFlags'} -> `Either NnError Integer' errorFromLength* #}
-- | no foreign (deallocate is managed by nanomq)
{#fun unsafe nn_send as nnSendDyn {socketToCInt `NnSocket', id `Ptr ()', withNnMSG- `Integer', cIntFromEnum `SndRcvFlags'} -> `Either NnError Integer' errorFromLength* #}

-- TODO fn with foreign does not make too much sense (should be in api)
{#fun unsafe nn_recv as nnRecvDyn' {socketToCInt `NnSocket', withNullPtr- `Ptr ()' id,  withNnMSG- `Integer', cIntFromEnum `SndRcvFlags'} -> `Either NnError Integer' errorFromLength* #}
{#fun unsafe nn_recv as nnRecvDyn {socketToCInt `NnSocket', withNullPtr- `ForeignPtr ()' foreignVoid*,  withNnMSG- `Integer', cIntFromEnum `SndRcvFlags'} -> `Either NnError Integer' errorFromLength* #}

-- TODO fn with foreign does not make too much sense (should be in api)
{#fun unsafe nn_recv as ^ {socketToCInt `NnSocket', withForeignPtr* `ForeignPtr ()', fromIntegral `Integer', cIntFromEnum `SndRcvFlags'} -> `Either NnError Integer' errorFromLength* #}
{#fun unsafe nn_recv as nnRecv' {socketToCInt `NnSocket', id `Ptr ()', fromIntegral `Integer', cIntFromEnum `SndRcvFlags'} -> `Either NnError Integer' errorFromLength* #}


{#fun unsafe nn_sendmsg as ^ {socketToCInt `NnSocket', withFmsghdr* `NnFMsghdr', cIntFromEnum `SndRcvFlags'} -> `Either NnError Integer' errorFromLength* #}
{#fun unsafe nn_sendmsg as nnSendmsg' {socketToCInt `NnSocket', fromMsghdr `NnMsghdr', cIntFromEnum `SndRcvFlags'} -> `Either NnError Integer' errorFromLength* #}

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
{#pointer * nn_iovec as NnFIovec foreign -> NnIovec #}
{#pointer * nn_msghdr as NnFMsghdr foreign -> NnMsghdr #}
{#pointer * nn_cmsghdr as NnFCmsghdr foreign -> NnCmsghdr #}
{#pointer * nn_iovec as NnIovec newtype #}
{#pointer * nn_msghdr as NnMsghdr newtype #}
{#pointer * nn_cmsghdr as NnCmsghdr newtype #}
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
iovecGetBase' (NnIovec v) = {#get nn_iovec.iov_base#} v
iovecGetBase :: NnFIovec -> IO(Ptr())
iovecGetBase = (`withForeignPtr` {#get nn_iovec.iov_base#})
iovecGetLen'' = (liftM toInteger) . {#get nn_iovec.iov_len#}
iovecGetLen' :: NnIovec -> IO(Integer)
iovecGetLen' (NnIovec v) = iovecGetLen'' v
iovecGetLen :: NnFIovec -> IO(Integer)
iovecGetLen = (`withForeignPtr` iovecGetLen'')
iovecSetLen'' v l = {#set nn_iovec.iov_len#} v (fromInteger l)
iovecSetLen' :: NnIovec -> Integer -> IO()
iovecSetLen' (NnIovec v) l = iovecSetLen'' v l
iovecSetLen :: NnFIovec -> Integer -> IO()
iovecSetLen p l = withForeignPtr p (`iovecSetLen''` l)
iovecSetBase'' v l = {#set nn_iovec.iov_base#} v l
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

msghdrGetiovlen'' = (liftM fromIntegral) . {#get nn_msghdr.msg_iovlen#}
msghdrGetiovlen' :: NnMsghdr -> IO(Int)
msghdrGetiovlen' (NnMsghdr v) = msghdrGetiovlen'' v
msghdrGetiovlen :: NnFMsghdr -> IO(Int)
msghdrGetiovlen = (`withForeignPtr` msghdrGetiovlen'')
msghdrSetiovlen'' v l = {#set nn_msghdr.msg_iovlen#} v (fromIntegral l)
msghdrSetiovlen' :: NnMsghdr -> Int -> IO()
msghdrSetiovlen' (NnMsghdr h) l = msghdrSetiovlen'' h l
msghdrSetiovlen ::  NnFMsghdr -> Int -> IO()
msghdrSetiovlen h v = withForeignPtr h (`msghdrSetiovlen''` v)

msghdrGetcontrol'' = {#get nn_msghdr.msg_control#}
msghdrGetcontrol' :: NnMsghdr -> IO(Ptr())
msghdrGetcontrol' (NnMsghdr v) = msghdrGetcontrol'' v
msghdrGetcontrol :: NnFMsghdr -> IO(Ptr())
msghdrGetcontrol = (`withForeignPtr` msghdrGetcontrol'')
msghdrSetcontrol'' v l = {#set nn_msghdr.msg_control#} v l
msghdrSetcontrol' :: NnMsghdr -> Ptr() -> IO()
msghdrSetcontrol' (NnMsghdr h) l = msghdrSetcontrol'' h l
msghdrSetcontrol ::  NnFMsghdr -> Ptr() -> IO()
msghdrSetcontrol h v = withForeignPtr h (`msghdrSetcontrol''` v)

msghdrGetcontrollen'' = (liftM toInteger) . {#get nn_msghdr.msg_controllen#}
msghdrGetcontrollen' :: NnMsghdr -> IO(Integer)
msghdrGetcontrollen' (NnMsghdr v) = msghdrGetcontrollen'' v
msghdrGetcontrollen :: NnFMsghdr -> IO(Integer)
msghdrGetcontrollen = (`withForeignPtr` msghdrGetcontrollen'')
msghdrSetcontrollen'' v l = {#set nn_msghdr.msg_controllen#} v (fromInteger l)
msghdrSetcontrollen' :: NnMsghdr -> Integer -> IO()
msghdrSetcontrollen' (NnMsghdr h) l = msghdrSetcontrollen'' h l
msghdrSetcontrollen ::  NnFMsghdr -> Integer -> IO()
msghdrSetcontrollen h v = withForeignPtr h (`msghdrSetcontrollen''` v)

cmsghdrGetlen'' = (liftM toInteger) . {#get nn_cmsghdr.cmsg_len#}
cmsghdrGetlen' :: NnMsghdr -> IO(Integer)
cmsghdrGetlen' (NnMsghdr v) = cmsghdrGetlen'' v
cmsghdrGetlen :: NnFMsghdr -> IO(Integer)
cmsghdrGetlen = (`withForeignPtr` cmsghdrGetlen'')
cmsghdrSetlen'' v l = {#set nn_cmsghdr.cmsg_len#} v (fromInteger l)
cmsghdrSetlen' :: NnMsghdr -> Integer -> IO()
cmsghdrSetlen' (NnMsghdr h) l = cmsghdrSetlen'' h l
cmsghdrSetlen ::  NnFMsghdr -> Integer -> IO()
cmsghdrSetlen h v = withForeignPtr h (`cmsghdrSetlen''` v)


cmsghdrGetlevel'' = (liftM fromIntegral) . {#get nn_cmsghdr.cmsg_level#}
cmsghdrGetlevel' :: NnMsghdr -> IO(Int)
cmsghdrGetlevel' (NnMsghdr v) = cmsghdrGetlevel'' v
cmsghdrGetlevel :: NnFMsghdr -> IO(Int)
cmsghdrGetlevel = (`withForeignPtr` cmsghdrGetlevel'')
cmsghdrSetlevel'' v l = {#set nn_cmsghdr.cmsg_level#} v (fromIntegral l)
cmsghdrSetlevel' :: NnMsghdr -> Int -> IO()
cmsghdrSetlevel' (NnMsghdr h) l = cmsghdrSetlevel'' h l
cmsghdrSetlevel ::  NnFMsghdr -> Int -> IO()
cmsghdrSetlevel h v = withForeignPtr h (`cmsghdrSetlevel''` v)

cmsghdrGettype'' = (liftM fromIntegral) . {#get nn_cmsghdr.cmsg_type#}
cmsghdrGettype' :: NnMsghdr -> IO(Int)
cmsghdrGettype' (NnMsghdr v) = cmsghdrGettype'' v
cmsghdrGettype :: NnFMsghdr -> IO(Int)
cmsghdrGettype = (`withForeignPtr` cmsghdrGettype'')
cmsghdrSettype'' v l = {#set nn_cmsghdr.cmsg_type#} v (fromIntegral l)
cmsghdrSettype' :: NnMsghdr -> Int -> IO()
cmsghdrSettype' (NnMsghdr h) l = cmsghdrSettype'' h l
cmsghdrSettype ::  NnFMsghdr -> Int -> IO()
cmsghdrSettype h v = withForeignPtr h (`cmsghdrSettype''` v)


-- TODO add align
newNnIovec = mallocBytes $ sizeOf (undefined::Ptr a) + {#sizeof size_t#}
newNnMsghdr = mallocBytes $ 2 * sizeOf (undefined::Ptr a) + sizeOf (undefined::CInt) + {#sizeof size_t#}
newNnCmsghdr = mallocBytes $ 2 * sizeOf (undefined::CInt) + {#sizeof size_t#}
newFNnIovec = mallocForeignPtrBytes $ sizeOf (undefined::Ptr a) + {#sizeof size_t#}
newFNnMsghdr = mallocForeignPtrBytes $ 2 * sizeOf (undefined::Ptr a) + sizeOf (undefined::CInt) + {#sizeof size_t#}
newFNnCmsghdr = mallocForeignPtrBytes $ 2 * sizeOf (undefined::CInt) + {#sizeof size_t#}


