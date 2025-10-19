module SpecTypingUtils (tests) where

import Test.Run
import Test.HUnit
import TypingUtils

tests :: NamedTests
tests = 
  [ ("GLB Laws", testGlbLaws)
  , ("LUB Laws", testLubLaws)
  ]



testGlbLaws :: Test
testGlbLaws = TestLabel "glb laws" $ TestList
  [ ]


testLubLaws :: Test
testLubLaws = TestLabel "дги laws" $ TestList
  [ ]