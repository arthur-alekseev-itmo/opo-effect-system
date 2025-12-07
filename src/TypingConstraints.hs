module TypingConstraints where

import Types
import TypingUtils

import qualified Data.Map as Map
import Data.Map ( Map, (!?) )
import qualified Data.List as List
import Control.Monad
import Control.Monad.Error.Class
import Data.Maybe
import qualified Data.Set as Set
import TypingCtx
import Data.Coerce
import qualified Debug.Trace


newtype TyConstraints n t = TyConstraints (Map n (TyConstraint t))
data TyConstraint t = TyConstraint { subOf :: [t], supOf :: [t] }

instance (Show t) => Show (TyConstraint t) where
  show TyConstraint { subOf, supOf } =
    show subOf <> " <: it <: " <> show supOf

instance (Show n, Show t) => Show (TyConstraints n t) where
  show (TyConstraints c) =
    List.intercalate "; " $ List.map (\(k, v) -> show k <> show v) $ Map.toList c

type Parameter = MonoTy
type Argument = MonoTy

data CombinedConstraints = CombinedConstraints 
  { tyConstraints :: TyConstraints TyName MonoTy
  , ltConstraints :: TyConstraints LtName Lt 
  }

instance (Ord n) => Semigroup (TyConstraints n t) where
  (TyConstraints l) <> (TyConstraints r) = TyConstraints $ Map.unionWith merge l r
    where  merge (TyConstraint l1 r1) (TyConstraint l2 r2) = TyConstraint (l1 <> l2) (r1 <> r2)

instance (Ord n) => Monoid (TyConstraints n t) where
  mempty = TyConstraints Map.empty

instance Semigroup CombinedConstraints where
  (CombinedConstraints llt lty) <> (CombinedConstraints rlt rty) =
    CombinedConstraints (llt <> rlt) (lty <> rty)

instance Monoid CombinedConstraints where
  mempty = CombinedConstraints mempty mempty

collectConstraints :: (TypingCtx m, LookupBound TyName (m Argument)) => PositionSign -> Parameter -> Argument -> m CombinedConstraints
collectConstraints position par arg = do
    ty <- tryAddType
    other <- deeper
    pure $ ty <> other
  where
    tryAddType = case (par, arg) of
      -- TODO: Any? #AA
      (_, TyVar _) -> pure mempty
      (TyVar name, bound) -> addTyConstraint name bound
      _ -> pure mempty
    deeper = case (par, arg) of
      (l, TyVar r) -> do
        res <- ?tyCtx `lookupBound` r
        collectConstraints position l res
      ( TyCtor MkTyCtor { name = "Any", lt = lt1, args = _ },
        TyCtor MkTyCtor { name = _, lt = lt2, args = _ } ) ->
        addLtConstraint lt1 lt2
      ( TyCtor MkTyCtor { name = ctor1, lt = lt1, args = args1 },
        TyCtor MkTyCtor { name = ctor2, lt = lt2, args = args2 } ) -> do
        -- TODO: Subtypes
        unless (ctor1 == ctor2) $
          throwError $ "Constructors don't match: " <> ctor1 <> " and " <> ctor2
        unless (length args1 == length args2) $
          throwError $ "Argument count mismatch: " <> show (length args1) <> " and " <> show (length args2)
        argConstraints <- zipWithM (collectConstraints InvariantPos) (coerce args1) (coerce args2)
        selfConstraints <- addLtConstraint lt1 lt2
        pure $ mconcat argConstraints <> selfConstraints
      ( TyFun MkTyFun { ctx = ctx1, lt = lt1, args = args1, res = res1 },
        TyFun MkTyFun { ctx = ctx2, lt = lt2, args = args2, res = res2 } ) -> do
          -- TODO: Subtypes
          unless (ctx1 == ctx2) $
            throwError $ "Context mismatch" <> show ctx1 <> " and " <> show ctx2
          unless (length args1 == length args2) $
            throwError $ "Argument count mismatch: " <> show (length args1) <> " and " <> show (length args2)
          -- TODO: Propagate contexts
          argConstraints <- zipWithM (collectConstraints $ changeSign position) (coerce args1) (coerce args2)
          resConstraints <- collectConstraints position (coerce res1) (coerce res2)
          selfConstraints <- addLtConstraint lt1 lt2
          pure $ mconcat argConstraints <> resConstraints <> selfConstraints
      (TyVar _, _) -> pure mempty
      (l, r) ->  throwError $ "Mismatching types (" <> show l <> ", " <> show r <> ")"
    addLtConstraint :: TypingCtx m => Lt -> Lt -> m CombinedConstraints
    addLtConstraint lt bound = case lt of
      (LtMin set) | [name] <- Set.toList set -> do
        new <- addConstraint name bound
        pure $ mempty { ltConstraints = new }
      _ -> pure mempty
    addTyConstraint name bound = do
      new <- addConstraint name bound
      pure $ mempty { tyConstraints = new }
    addConstraint :: (TypeType t, Ord n, TypingCtx m) => n -> t -> m (TyConstraints n t)
    addConstraint name bound = pure $ TyConstraints $ Map.singleton name $ case position of
      PositivePos -> TyConstraint [bound] []
      NegativePos -> TyConstraint [] [bound]
      InvariantPos -> TyConstraint [bound] [bound] 

solveConstraint :: (TypeType ty, TypingCtx m) => TyConstraint ty -> m ty
solveConstraint c@(TyConstraint subs sups) = do
  let super = lubAll sups
  let sub = glbAll subs
  unless (sub `subTyOf` super) $ throwError $ "Cant solve constraint" <> show c
  pure $ case (subs, sups) of
    ([], _ : _) -> super
    _ -> sub

solveFor :: (TypeType ty, TypingCtx m, Ord n) => TyConstraints n ty -> n -> m ty
solveFor (TyConstraints constraints) name = do
  let solved = solveConstraint <$> constraints !? name
  fromMaybe (pure top) solved