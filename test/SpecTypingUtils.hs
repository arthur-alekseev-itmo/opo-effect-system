module SpecTypingUtils (tests) where

import Data.Function
import Test.Run
import Test.HUnit
import Test.QuickCheck qualified as QC
import Test.QuickCheck ( (==>) )
import Test.QuickCheck.Classes.Base qualified as QC
import TypingUtils
import Types
import Arbitrary.MonoTypes


tests :: NamedTests
tests = []
  -- [ ("GLB Laws", testGlbLaws)
  -- , ("LUB Laws", testLubLaws)
  -- ]

testGlbLaws :: Test
testGlbLaws = TestList
 [ propertyToTest "forall x => glb(x, x) = x" \(x :: MonoTy) -> let ?tyCtx = [] in x `glb` x == x
 , propertyToTest "x <: y => glb(x, y) = x" \(Subtyped (x, y)) -> let ?tyCtx = [] in x `glb` y == x
 ]

testLubLaws :: Test
testLubLaws = TestList
 [ propertyToTest "x <: y => lub(x, y) = y" \(Subtyped (x, y)) -> let ?tyCtx = [] in x `lub` y == y
 , propertyToTest "forall x => lub(x, x) = x" \(x :: MonoTy) -> let ?tyCtx = [] in x `lub` x == x
 ]