import Test.HUnit

import Test.Run
import SpecTypingUtils
import SpecInference
import qualified Data.Foldable as List


allTests :: IO NamedTests
allTests = mconcat
  [ SpecTypingUtils.tests
  , SpecInference.tests
  ]

main :: IO ()
main = allTests >>= testMain