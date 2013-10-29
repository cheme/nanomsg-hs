{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.NanoMsg.C.NanoMsgStruct where
 
import Foreign
import Foreign.C.Types
import Foreign.C.String
#include "nanomsg/nn.h"
#include "inlinemacro.h"


nN_MSG = #const NN_MSG

data NNIoVec = NNIoVec
  { iobase :: {-# UNPACK #-} !CString
  , ioleng :: {-# UNPACK #-} !CSize
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


data NNMsgHdr = NNMsgHdr
  { msgvecs :: {-# UNPACK #-} !(Ptr NNIoVec)
  , msglvec :: {-# UNPACK #-} !CInt
  , msgctrl :: {-# UNPACK #-} !(Ptr ())
  , msglctr :: {-# UNPACK #-} !(CSize)
  }

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
    #{poke nn_msghdr, msg_control} ptr cs
    #{poke nn_msghdr, msg_controllen} ptr cl

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

