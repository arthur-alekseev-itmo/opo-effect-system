import Test.HUnit

import Test.Run
import SpecTypingUtils
import SpecInference
import qualified Data.Foldable as List


allTests :: NamedTests
allTests = List.concat
  [ SpecTypingUtils.tests
  , SpecInference.tests
  ]

main :: IO ()
main = testMain allTests