import RtmAPISpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  context "rtm API Tests" $ do
    rtmAPISpec

