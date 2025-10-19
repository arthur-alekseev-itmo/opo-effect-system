module Test.Run where

import Test.HUnit.Base qualified as HU
import Test.QuickCheck qualified as QC
import Control.Monad (forM_)
import Text.Printf (printf)

type NamedTests = [(String, HU.Test)]

testMain :: NamedTests -> IO ()
testMain tests = do
  forM_ tests runTest
  where
    runTest (name, test) = do
      printf "Running: %s\n" name
      (counts, _) <- HU.performTest reportStart reportError reportFailure () test
      printf "  Ok: %d/%d\n" (HU.failures counts) (HU.cases counts)
    reportStart _ _ = pure ()
    reportError _ msg HU.State{} () = printf "  Err: %s\n" msg
    reportFailure _ msg HU.State{} () = printf "  Fail: %s\n" msg