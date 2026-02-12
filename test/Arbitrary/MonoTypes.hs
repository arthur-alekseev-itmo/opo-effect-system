module Arbitrary.MonoTypes where

import Data.Function
import Test.Run
import Test.HUnit
import Test.QuickCheck qualified as QC
import Test.QuickCheck ( (==>) )
import Test.QuickCheck.Classes.Base qualified as QC
import Types
import TypingUtils

instance QC.Arbitrary MonoTy where
  arbitrary = QC.sized $ fix \rec -> \case
    0 -> do
      lt <- arbitraryLt
      pure $ TyCtor MkTyCtor { name = "Any", lt, args = [] }
    n -> QC.oneof [arbitraryCtor rec n, arbitraryFn rec n]
    where
      arbitraryLt = QC.elements [LtLocal, ltFree]
      arbitraryCtor rec n = do
        lt <- arbitraryLt
        args <- QC.listOf $ rec (n - 1)
        pure $ TyCtor MkTyCtor { name = "K", lt , args}
      arbitraryFn rec n = do
        lt <- arbitraryLt
        args <- QC.listOf $ rec (n - 1)
        res <- rec (n - 1)
        pure $ TyFun MkTyFun { ctx = [], lt, args, res }

newtype Subtyped = Subtyped (MonoTy, MonoTy)
  deriving Show

-- (a, b) => a <: b
instance QC.Arbitrary Subtyped where
  arbitrary = QC.sized $ fix \rec -> \case
    0 -> do
      (llt, rlt) <- arbitraryLts
      let fres = TyCtor MkTyCtor { name = "Any", lt = llt, args = [] }
      let sres = TyCtor MkTyCtor { name = "Any", lt = rlt, args = [] }
      pure $ Subtyped (fres, sres)
    n -> QC.oneof [arbitraryCtor rec n, arbitraryFn rec n]
    where
      sfst (Subtyped (x, _)) = x
      ssnd (Subtyped (_, x)) = x
      arbitraryLts = do QC.elements [(ltFree, LtLocal), (LtLocal, LtLocal), (ltFree, ltFree)]
      arbitraryCtor rec n = do
        (_, rlt) <- arbitraryLts
        args <- QC.listOf $ rec (n - 1)
        let fres = TyCtor MkTyCtor { name = "K", lt = rlt, args = map sfst args }
        let sres = TyCtor MkTyCtor { name = "K", lt = rlt, args = map ssnd args }
        pure $ Subtyped (fres, sres)
      arbitraryFn rec n = do
        (llt, rlt) <- arbitraryLts
        args <- QC.listOf $ rec (n - 1)
        Subtyped (lres, rres) <- rec (n - 1)
        let fres = TyFun MkTyFun { ctx = [], lt = llt, args = map ssnd args, res = lres }
        let sres = TyFun MkTyFun { ctx = [], lt = rlt, args = map sfst args, res = rres }
        pure $ Subtyped (fres, sres)