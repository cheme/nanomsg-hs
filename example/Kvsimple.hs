-- |
-- kvsimple - simple key-value message monad for example applications

module Kvsimple
  (
      KVMsg(..)
    , store
    , send
    , recv
    , dump
    -- for test only
    , test_kvmsg
  ) where

import qualified Data.HashMap.Lazy as H
import Data.Hashable(Hashable)
import qualified System.NanoMsg as N
import System.NanoMsg(Socket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
-- for binary serialization 
import Data.Binary

-- |
--  Message is not formatted on wire as 3 frames:
--  using haskell binary instead.
--  frame 0: key (0MQ string)
--  frame 1: sequence (8 bytes, network order)
--  frame 2: body (blob)
data KVMsg k v = KVMsg 
  { key :: Maybe k
  , sequence :: Int
  , body :: Maybe v
  }
  deriving (Show)



instance (Binary k, Binary v) => Binary (KVMsg k v) where
    put (KVMsg k s b) = do put k
                           put s
                           put b
    get = do k <- get
             s <- get
             b <- get
             return $ KVMsg k s b

-- Store me in a dict if I have anything to store
store :: (Eq k, Hashable k) => KVMsg k v -> H.HashMap k v -> H.HashMap k v
store (KVMsg (Just k) s (Just v)) map = H.insert k v map
store _ m = m


-- Send key-value message to socket; any empty frames are sent as such.
send :: (N.Sender a, Binary k, Binary v) => KVMsg k v -> Socket a -> IO ()
send kvm sock = N.send sock []  ((BS.pack . LBS.unpack . encode) kvm) >> return ()


-- Reads key-value message from socket, returns new kvmsg instance.
recv :: (N.Receiver a, Binary k, Binary v) => Socket a -> IO (KVMsg k v)
recv sock = N.receive sock [] >>= return . decode . LBS.pack . BS.unpack

-- using haskell show.
dump :: (Show k, Show v) => KVMsg k v -> String
dump = show

-- It's good practice to have a self-test method that tests the class; this
test_kvmsg :: Bool -> IO ()
test_kvmsg verbose = do 
    putStrLn " * kvmsg: "
    -- Prepare our context and sockets
    output <- N.socket N.Pair
    N.bind output "ipc://kvmsg_selftest.ipc"
    input <- N.socket N.Pair
    N.connect input "ipc://kvmsg_selftest.ipc"
    
    let kvmap = H.empty
    -- Test send and receive of simple message
    let kvmsg = KVMsg (Just "key") 1 (Just "body")
    ifverbose $ dump kvmsg
    send kvmsg output
    let kvmap2 = store kvmsg kvmap
    
    kvmsg2 <- recv input
     
    ifverbose $ dump kvmsg2

    assert (key kvmsg2 == (Just "key"))

    let kvmap3 = store kvmsg2 kvmap2

    assert (H.size kvmap3 == 1) -- shouldn't be different
    putStrLn "OK"
  where ifverbose s = if verbose == True then putStrLn s else return ()
        assert False      = putStrLn "Failed assertion"
        assert _          = return ()

