import Api
import C
import Test.QuickCheck
import Test.HUnit
import System.NanoMsg

main :: IO ()
main = do 
  tests stdArgs{maxSuccess=50} [
    ("Send/Receive", property $ prop_send_receive Req Rep)
    , ("Get/SetOptions", property $ prop_set_get_options)
    ]
  testNnSocket >>= runTestTT
  testNnClose  >>= runTestTT
  testNnOptions >>= runTestTT
  testNnBindConnect >>= runTestTT
  testNnSendReceiveStatic >>= runTestTT
  testNnSendReceiveDyn >>= runTestTT
  testNnCmsg >>= runTestTT
  testNnSendReceiveMsg >>= runTestTT
  testNnSendReceiveMsgHdr >>= runTestTT
  return ()

