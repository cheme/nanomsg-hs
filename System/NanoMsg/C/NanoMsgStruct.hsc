{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.NanoMsg.C.NanoMsgStruct where
 

import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified Data.ByteString as BS
import Data.List(foldl')
import Control.Monad(foldM_)
import Foreign.Storable(pokeElemOff)
#include "nanomsg/nn.h"
#include "inlinemacro.h"


-- not c2hs (to type it explicitly as a funPtr for finalizer) and needed by struct
foreign import ccall "System/NanoMsg/C/NanoMsg.chs.h &nn_freemsg"
   nnFunPtrFreeMsg :: FunPtr (Ptr () -> IO ())

nN_MSG :: CInt
nN_MSG = #const NN_MSG

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
  , msgctrl :: {-# UNPACK #-} !(Ptr ())
  , msglctr :: {-# UNPACK #-} !(CSize)
  }

data NNFMsgHdr = NNFMsgHdr
  { msgfvecs :: !([NNFIoVec])
  , msgflvec :: {-# UNPACK #-} !CInt
  , msgfctrl :: {-# UNPACK #-} !(ForeignPtr ())
  , msgflctr :: {-# UNPACK #-} !(CSize)
  }

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

