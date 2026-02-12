module Test.Run where

import Control.Exception
import Test.HUnit.Base qualified as HU
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Classes.Base qualified as QC
import Control.Monad (forM_)
import Text.Printf (printf)
import Test.QuickCheck (Result(output))

type NamedTests = [(String, HU.Test)]

propertyToTest :: QC.Testable prop => String -> prop -> HU.Test
propertyToTest description = propertyToTestIO description . pure

propertyToTestIO :: QC.Testable prop => String -> IO prop -> HU.Test
propertyToTestIO description property = HU.TestCase $
  property >>= QC.quickCheckWithResult args >>= \case
    QC.Failure { theException = Just exception } ->
      HU.assertFailure $ "tested " <> description <> ", but\n" <> show exception
    QC.Failure { theException = Nothing, output } ->
      HU.assertFailure $ "tested " <> description <> ", but\n" <> output
    _ -> pure ()
  where
    args = QC.stdArgs { QC.chatty = True, QC.maxSuccess = 150, QC.maxSize = 2 }

testMain :: NamedTests -> IO ()
testMain tests = do
  forM_ tests runTest
  where
    runTest (name, test) = do
      printf "Running: %s\n" name
      (counts, _) <- HU.performTest reportStart reportError reportFailure () test
      printf "  Ok: %d/%d\n" (HU.cases counts - HU.failures counts) (HU.cases counts)
    reportStart _ _ = pure ()
    reportError _ msg HU.State{} () = printf "  Err: %s\n" msg
    reportFailure _ msg HU.State{} () = printf "  Fail: %s\n" msg