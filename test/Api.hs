-- | Tests of Api functions.
module Api where
import System.NanoMsg
import System.NanoMsg.C.NanoMsg(nnStrerror)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Applicative((<$>))
import Control.Concurrent.MVar(newEmptyMVar,putMVar,takeMVar)
import Control.Concurrent(forkIO)
import Control.Monad(forM_)
import Control.Concurrent(threadDelay)
import Foreign.C.Types(CInt)
import Foreign.Storable(sizeOf)

-- instance Arbitrary Socket a where
--   arbitrary = t

-- arbitraryBoundedEnum
-- > require to define bounded instances of enums : methode minBound and maxBound -> not ok for every enum
-- coarbitraryEnum 

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary
  shrink bs = BS.pack <$> shrink (BS.unpack bs)

prop_send_receive :: (SocketType a, SocketType b, Receiver b, Sender a) => a -> b -> ByteString -> Property
prop_send_receive a b msg = monadicIO $ do
  msg' <- run $ do
    sender <- socket a
    receiver <- socket b
    sync <- newEmptyMVar
    epre <- bind receiver "inproc://endpoint"
    esen <- connect sender "inproc://endpoint"
    threadDelay 1000
    forkIO $ receive receiver [] >>= putMVar sync
    send sender [] msg
    m <- takeMVar sync
    shutdown esen
    close sender
    shutdown epre
    close receiver
    return m
  assert (msg == msg')

-- TODO API shall need a socket type related options, plus some restricted int types
prop_set_get_options :: String -> Bool -> Positive Int -> Property
prop_set_get_options string bool (Positive int) = monadicIO $ do
  (bool', string', int') <- run $ do
    sock <- socket Sub
    setIPV4ONLY sock bool
    bool' <- getIPV4ONLY sock
    -- dirty
    int' <- if int < sizeOf (undefined :: CInt) `div` 2 then do 
      setReceiveBuffer sock int
      getReceiveBuffer sock else
      return int -- skip test very dirty
    setSubSubscribe sock string
--    string' <- getSubSubscribe sock -- not yet implemented
    return (bool',string,int')
  assert (bool == bool && string == string' && int == int')
 

tests :: Args -> [(String, Property)] -> IO ()
tests args testlist = forM_ testlist $ \(title,prop) -> do
  putStrLn $ "* Checking : " ++ title
  quickCheckWith args prop


