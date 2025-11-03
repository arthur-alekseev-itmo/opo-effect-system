module SpecInference (tests) where

import Data.Function
import Test.Run
import Test.HUnit
import TypingUtils
import Types
import Arbitrary.MonoTypes
import Driver
import Data.Map ((!?))

tests :: NamedTests
tests =
  [ ("Lifetimes inferenced in id", inferenceLtsId)
  , ("Lifetimes inferenced in compose", inferenceLtsCompose)
  , ("Lifetimes inferenced in lazy map", inferenceLtsLazyMap)
  ]

ltsInferenceTest :: String -> String -> String -> Assertion
ltsInferenceTest annotated empty fname = do
  annotatedTypes <- typesOf annotated
  emptyTypes <- typesOf empty
  let result = Just (==) <*> annotatedTypes <*> emptyTypes
  logResults annotatedTypes emptyTypes
  assertBool "Programs inferred type differs" (result == Just True)
  where
    typesOf program = do
      let prog = parseProg program
      let (effCtx, tyCtx) = collectDecls prog
      types <- typecheck effCtx tyCtx prog
      pure $ types !? fname
    logResults f s = do
      putStr "Expedcted: "
      print f
      putStr "Actual:    "
      print s

inferenceLtsId :: Test
inferenceLtsId = TestCase $ ltsInferenceTest
  "  fun id[lr]<a <: Any'lr>(x: a): a = x \
   \ let r = id[free]<Int'free>(3)"
  "  fun id[lr]<a <: Any'lr>(x: a): a = x \
   \ let r = id<Int'free>(3)"
  "r"

inferenceLtsCompose :: Test
inferenceLtsCompose = TestCase $ ltsInferenceTest
  "  fun id[lr]<a <: Any'lr>(x: a): a = x \
   \ fun compose \
   \ [la, lb, lc, lf, lg] \
   \ <a <: Any'la, b <: Any'lb, c <: Any'lc> \
   \ (f: (b)'lf -> c, g: (a)'lg -> b): (a)'lb+lf+lg -> c = \ 
   \ fun(x : a) f(g(x)) \
   \ \
   \ let c = compose \
   \ [free, free, free, free, free] \
   \ <Any'free, Any'free, Any'free> \
   \ (fun (x: Any'free) id[free]<Any'free>(x), fun (x: Any'free) id[free]<Any'free>(x))"
  "  fun id[lr]<a <: Any'lr>(x: a): a = x \
   \ fun compose \
   \ [la, lb, lc, lf, lg] \
   \ <a <: Any'la, b <: Any'lb, c <: Any'lc> \
   \ (f: (b)'lf -> c, g: (a)'lg -> b): (a)'lb+lf+lg -> c = \ 
   \ fun(x : a) f(g(x)) \
   \ \
   \ let c = compose \
   \ <Any'free, Any'free, Any'free> \
   \ (fun (x: Any'free) id<Any'free>(x), fun (x: Any'free) id<Any'free>(x))"
  "c"

inferenceLtsLazyMap :: Test
inferenceLtsLazyMap = TestCase $ ltsInferenceTest
  "  fun id[lr]<a <: Any'lr>(x: a): a = x \
   \ data LazyList<a> \
   \     = LNil \
   \     | LCons[lh, lt, ll](()'lh -> a, ()'lt -> LazyList<a>'ll) \
   \ \
   \ fun lazyMap \
   \     [lf, la, lb]<a <: Any'la, b <: Any'lb> \
   \     (xs: LazyList<a>, f: (a)'lf -> b): LazyList<b>'lf+la = \
   \     match xs { \
   \         case LNil() -> LNil<b>() \
   \         case LCons(h, t) -> LCons[lf+la, lf+la, lf+la]<b>( \
   \             fun() f(h()), \
   \             fun() lazyMap[lf, la, lb]<a, b>(t(), f) \
   \         ) \
   \     } \
   \ \
   \ let r = lazyMap[free, free, free]<Int'free, Int'free>(LNil<Int'free>(), fun (x: Int'free) id[free]<Int'free>(x))"
  "  fun id[lr]<a <: Any'lr>(x: a): a = x \
   \ data LazyList<a> \
   \     = LNil \
   \     | LCons[lh, lt, ll](()'lh -> a, ()'lt -> LazyList<a>'ll) \
   \ \
   \ fun lazyMap \
   \     [lf, la, lb]<a <: Any'la, b <: Any'lb> \
   \     (xs: LazyList<a>, f: (a)'lf -> b): LazyList<b>'lf+la = \
   \     match xs { \
   \         case LNil() -> LNil<b>() \
   \         case LCons(h, t) -> LCons[lf+la, lf+la, lf+la]<b>( \
   \             fun() f(h()), \
   \             fun() lazyMap[lf, la, lb]<a, b>(t(), f) \
   \         ) \
   \     } \
   \ \
   \ let r = lazyMap<Int'free, Int'free>(LNil<Int'free>(), fun (x: Int'free) id<Int'free>(x))"
   "r"

-- TODO: Tests from files
-- TODO: Check inferred values