module SpecInference (tests) where

import Data.Function
import Test.Run
import Test.HUnit
import TypingUtils
import Types
import Arbitrary.MonoTypes
import Driver
import Data.Map ((!?))
import Data.Maybe (isJust)

tests :: IO NamedTests
tests = sequenceA
  [ -- ("Lifetimes inferenced in id",) <$> inferenceLtsId
  -- , ("Lifetimes inferenced in compose",) <$> inferenceLtsCompose
   ("Lifetimes inferenced in lazy map",) <$> inferenceLtsLazyMap
  ]

inferenceTest :: String -> IO Assertion
inferenceTest file = do
  prog <- parseProg <$> readFile ("test/Samples/" <> file)
  let (effCtx, tyCtx) = collectDecls prog
  types <- typecheck effCtx tyCtx prog
  let explicit = types !? "rexplicit"
  let inferredLt = types !? "rltinfer"
  let inferredTy = types !? "rtyinfer"
  let exists = isJust explicit
  let equal = explicit == inferredLt && explicit == inferredTy
  pure $ assertBool "Programs inferred type differs " (equal && exists)

inferenceLtsId :: IO Test
inferenceLtsId = TestCase <$> inferenceTest "id.co"

inferenceLtsCompose :: IO Test
inferenceLtsCompose = TestCase <$> inferenceTest "compose.co"

inferenceLtsLazyMap :: IO Test
inferenceLtsLazyMap = TestCase <$> inferenceTest "lazymap.co"
