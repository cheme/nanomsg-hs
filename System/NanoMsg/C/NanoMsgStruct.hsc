{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.NanoMsg.C.NanoMsgStruct where
 

import Foreign
import Foreign.C.Types
import Foreign.C.String
#include "nanomsg/nn.h"
#include "inlinemacro.h"


-- not c2hs (to type it explicitly as a funPtr for finalizer) and needed by struct
foreign import ccall "System/NanoMsg/C/NanoMsg.chs.h &nn_freemsg"
   nnFunPtrFreeMsg :: FunPtr (Ptr () -> IO ())

-- For message hdr, current nanomsg (alpha) store only dynamic length header -> we add a small implementation for static length which explain some strange serialization to nN_MSG : see comment with @1


nN_MSG :: CInt
nN_MSG = #const NN_MSG

nN_DONTWAIT :: CInt
nN_DONTWAIT = #const NN_DONTWAIT


data NNIoVec = NNIoVec
  { iobase :: {-# UNPACK #-} !CString
  , ioleng :: {-# UNPACK #-} !CSize
  }

data NNFIoVec = NNFIoVec
  { iofbase :: {-# UNPACK #-} !(ForeignPtr CChar)
  , iofleng :: {-# UNPACK #-} !CSize
  }

instance Storable NNIoVec where
  alignment _ = #{alignment nn_iovec}
  sizeOf _ = #{size nn_iovec}
  peek ptr = do
    base <- #{peek nn_iovec, iov_base} ptr
    leng <- #{peek nn_iovec, iov_len} ptr
    return $ NNIoVec base leng
  poke ptr (NNIoVec base leng) = do
    #{poke nn_iovec, iov_base} ptr base
    #{poke nn_iovec, iov_len} ptr leng

instance Storable NNFIoVec where
  alignment _ = #{alignment nn_iovec}
  sizeOf _ = #{size nn_iovec}
  peek ptr = do
    leng <- #{peek nn_iovec, iov_len} ptr
    if leng == (fromIntegral nN_MSG) then do
      base <- #{peek nn_iovec, iov_base} ptr >>= newForeignPtr nnFunPtrFreeMsg
      return $ NNFIoVec (castForeignPtr base) leng
    else do
      base <- #{peek nn_iovec, iov_base} ptr >>= newForeignPtr finalizerFree -- TODO array of finalizer instead??
      return $ NNFIoVec base leng
  poke ptr (NNFIoVec base leng) = do
    withForeignPtr base (\b -> #{poke nn_iovec, iov_base} ptr b)
    #{poke nn_iovec, iov_len} ptr leng

data NNMsgHdr = NNMsgHdr
  { msgvecs :: {-# UNPACK #-} !(Ptr NNIoVec)
  , msglvec :: {-# UNPACK #-} !CInt
  , msgctrl :: {-# UNPACK #-} !(Ptr NNCMsgHdr)
  , msglctr :: {-# UNPACK #-} !(CSize)
  }

data NNFMsgHdr = NNFMsgHdr
  { msgfvecs :: !([NNFIoVec])
  , msgflvec :: {-# UNPACK #-} !CInt
  , msgfctrl :: {-# UNPACK #-} !(ForeignPtr NNCMsgHdr)
  , msgflctr :: {-# UNPACK #-} !(CSize)
  }

-- Note that peek on NNFMsgHdr and NNMsgHdr is not use due to nanomsg receive working (already instantiated when receiving).

instance Storable NNFMsgHdr where
  alignment _ = #{alignment nn_msghdr}
  sizeOf _ = #{size nn_msghdr}
  peek ptr = do
    vl <- #{peek nn_msghdr, msg_iovlen} ptr
    vs <- #{peek nn_msghdr, msg_iov} ptr >>= peekArray (fromIntegral vl)
    cs <- #{peek nn_msghdr, msg_control} ptr >>= newForeignPtr finalizerFree
    cl <- #{peek nn_msghdr, msg_controllen} ptr
    return $ NNFMsgHdr vs vl cs cl
  poke ptr (NNFMsgHdr vs vl cs cl) = do
    ar <- mallocArray (fromIntegral vl)
    pokeArray ar vs
    #{poke nn_msghdr, msg_iov} ptr ar
    #{poke nn_msghdr, msg_iovlen} ptr vl
    withForeignPtr cs (\c -> #{poke nn_msghdr, msg_control} ptr c)
    #{poke nn_msghdr, msg_controllen} ptr cl

instance Storable NNMsgHdr where
  alignment _ = #{alignment nn_msghdr}
  sizeOf _ = #{size nn_msghdr}
  peek ptr = do
    vs <- #{peek nn_msghdr, msg_iov} ptr
    vl <- #{peek nn_msghdr, msg_iovlen} ptr
    cs <- #{peek nn_msghdr, msg_control} ptr
    cl <- #{peek nn_msghdr, msg_controllen} ptr
    return $ NNMsgHdr vs vl cs cl
  poke ptr (NNMsgHdr vs vl cs cl) = do
    #{poke nn_msghdr, msg_iov} ptr vs
    #{poke nn_msghdr, msg_iovlen} ptr vl
    if (cl == (-1)) then
      #{poke nn_msghdr, msg_control} ptr cs
    else -- poke multi hdr (don't use pokeArray (not constant size elements))
      #{poke nn_msghdr, msg_control} ptr cs
    if (cl == 0) then
      #{poke nn_msghdr, msg_controllen} ptr cl
    else
      #{poke nn_msghdr, msg_controllen} ptr (fromIntegral nN_MSG :: CSize) -- @1

data NNCMsgHdr = NNCMsgHdr
  { cmsglen :: {-# UNPACK #-} !(CSize)
  , cmsglev :: {-# UNPACK #-} !(CInt)
  , cmsgtyp :: {-# UNPACK #-} !(CInt)
  }

instance Storable NNCMsgHdr where
  alignment _ = #{alignment nn_msghdr}
  sizeOf _ = #{size nn_cmsghdr}
  peek ptr = do
    hl <- #{peek nn_cmsghdr, cmsg_len} ptr
    le <- #{peek nn_cmsghdr, cmsg_level} ptr
    ty <- #{peek nn_cmsghdr, cmsg_type} ptr
    return $ NNCMsgHdr hl le ty
  poke ptr (NNCMsgHdr hl le ty) = do
    #{poke nn_cmsghdr, cmsg_len} ptr hl
    #{poke nn_cmsghdr, cmsg_level} ptr le
    #{poke nn_cmsghdr, cmsg_type} ptr ty

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

