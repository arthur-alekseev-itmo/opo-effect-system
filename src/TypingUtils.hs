module TypingUtils where

import Common
import Syntax
import Types
import TypingCtx

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Foldable
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Maybe
import Optics
import Data.Kind
import GHC.Stack


class DoSubst target where
  onLt :: Subst target -> String -> Lt -> Lt
  onMonoTy :: Subst target -> String -> MonoTy -> MonoTy

instance DoSubst Lt where
  onLt (Subst subst) name or = Map.findWithDefault or name subst
  onMonoTy _ _ = id

instance DoSubst MonoTy where
  onLt _ _ = id
  onMonoTy (Subst subst) name or = Map.findWithDefault or name subst


newtype Subst target = Subst { getSubst :: Map String target }
  deriving newtype Show

mkSubst :: (MonadError String m, HasCallStack) => [String] -> [target] -> m (Subst target)
mkSubst names target = do
  unless (length names == length target) $
    throwError $ "Unexpected number of parameters from\n" <> prettyCallStack callStack
  pure $ Subst $ Map.fromList $ zip names target

mkSubst2
  :: (MonadError String m, HasCallStack)
  => [String] -> [target] -> [String] -> [target] -> m (Subst target)
mkSubst2 names1 target1 names2 target2 = do
  unless (length names1 == length target1 && length names2 == length target2) $
    throwError $ "Unexpected number of parameters from\n" <> prettyCallStack callStack
  pure $ Subst $ Map.fromList $ zip names1 target1 ++ zip names2 target2

instance DoSubst target => Apply (Subst target) Lt Lt where
  f @ arg = case arg of
    LtMin l r -> LtMin (f @ l) (f @ r)
    LtMax l r -> LtMax (f @ l) (f @ r)
    LtVar name -> onLt f name (ltVar name)
    e -> e

    -- LtLocal -> LtLocal
    -- LtMin names -> foldr lub ltFree $ (\name -> onLt f name (ltVar name)) <$> Set.toList names
    -- LtStar -> LtStar


instance DoSubst target => Apply (Subst target) MonoTy MonoTy where
  f @ arg = case arg of
    TyVar name -> onMonoTy f name arg
    TyCtor ctor -> TyCtor $ f @ ctor
    TyFun MkTyFun { ctx, lt, args, res } -> TyFun MkTyFun { ctx = f @ ctx, lt = f @ lt, args = f @ args, res = f @ res}

instance DoSubst target => Apply (Subst target) TyCtor TyCtor where
  f @ MkTyCtor { name, lt, args } = MkTyCtor { name, lt = f @ lt, args = f @ args }

instance DoSubst target => Apply (Subst target) Expr Expr where
  f @ arg = case arg of
    Const _ -> arg
    Plus lhs rhs -> Plus (f @ lhs) (f @ rhs)
    Var _ -> arg
    TLam {} -> error "TLam is not supported" -- TODO
    TApp MkTApp { lhs, ltArgs, tyArgs } -> TApp MkTApp { lhs = f @ lhs, ltArgs = f @ ltArgs, tyArgs = f @ tyArgs }
    CapCtor MkCapCtor { name, tyArgs, marker, handler } -> CapCtor MkCapCtor { name, tyArgs = f @ tyArgs, marker, handler = f @ handler }
    Lam MkLam { ctxParams, params, body } -> Lam MkLam { ctxParams = f @ ctxParams, params = f @ params, body = f @ body }
    App MkApp { callee, ctxArgs, args } -> App MkApp { callee = f @ callee, ctxArgs = f @ ctxArgs, args = f @ args }
    Match MkMatch { scrutinee, branches } -> Match MkMatch { scrutinee = f @ scrutinee, branches = f @ branches }
    Perform MkPerform { opName, cap, tyArgs, args } -> Perform MkPerform { opName, cap = f @ cap, tyArgs = f @ tyArgs, args = f @ args }
    Handle MkHandle { capName, effTy, handler, body } -> Handle MkHandle { capName, effTy = f @ effTy, handler = f @ handler, body = f @ body }
    RtHandler MkRtHandler { marker, body } -> RtHandler MkRtHandler { marker, body = f @ body }

instance DoSubst target => Apply (Subst target) HandlerEntry HandlerEntry where
  f @ MkHandlerEntry { opName, tyParams, paramNames, body } = MkHandlerEntry { opName, tyParams, paramNames, body = f @ body } -- todo

instance DoSubst target => Apply (Subst target) Param Param where
  f @ MkParam { name, ty } = MkParam { name, ty = f @ ty }

instance DoSubst target => Apply (Subst target) Branch Branch where
  f @ MkBranch { ctorName, varPatterns, body } = MkBranch { ctorName, varPatterns, body = f @ body }

instance DoSubst target => Apply (Subst target) OpSig OpSig where
  f @ MkOpSig { tyParams, args, res } = MkOpSig { tyParams, args, res = f @ res } -- TODO


infix 5 `lub`
class LeastUpperBound ty where
  type LubC ty :: Constraint
  lub :: (LubC ty, HasCallStack) => ty -> ty -> ty

instance LeastUpperBound Lt where
  type LubC Lt = ()
  lub LtLocal _ = LtLocal
  lub _ LtLocal = LtLocal
  lub LtStar _ = LtStar
  lub _ LtStar = LtStar
  lub l r = LtMin l r

instance LeastUpperBound MonoTy where
  type LubC MonoTy = (?tyCtx :: TyCtx)
  lub var@(TyVar name1) (TyVar name2)
    | name1 == name2 = var
    | otherwise = ?tyCtx `lookupBound` name1 `lub` ?tyCtx `lookupBound` name2
  lub
    (TyCtor MkTyCtor { name = name1, lt = lt1, args = args1 })
    (TyCtor MkTyCtor { name = name2, lt = lt2, args = args2 })
    | name1 == name2 && args1 == args2 =
      TyCtor MkTyCtor { name = name1, lt = lt1 `lub` lt2, args = args1 }
  lub
    (TyFun MkTyFun { ctx = ctx1, lt = lt1, args = args1, res = res1 })
    (TyFun MkTyFun { ctx = ctx2, lt = lt2, args = args2, res = res2 })
    | ctx1 == ctx2 && length args1 == length args2 =
      TyFun MkTyFun { ctx = ctx1, lt = lt1 `lub` lt2, args = zipWith glb args1 args2, res = res1 `lub` res2 }
  lub ty1 ty2 =
    let lt = lubAll (ty1 `ltsAt` PositivePos) `lub` lubAll (ty2 `ltsAt` PositivePos) in
    TyCtor MkTyCtor { name = "Any", lt, args = [] }

lubAll :: Foldable f => f Lt -> Lt
lubAll = foldr lub ltFree


infix 5 `glb`
class GreatestLowerBound ty where
  type GlbC ty :: Constraint
  glb :: (GlbC ty, HasCallStack) => ty -> ty -> ty

instance GreatestLowerBound Lt where
  type GlbC Lt = ()
  glb LtLocal l = l
  glb l LtLocal = l
  glb LtStar _ = LtStar
  glb _ LtStar = LtStar
  glb l r = LtMax l r

instance GreatestLowerBound MonoTy where
  type GlbC MonoTy = (?tyCtx :: TyCtx)
  glb var@(TyVar name1) (TyVar name2)
    | name1 == name2 = var
    | otherwise = ?tyCtx `lookupBound` name1 `glb` ?tyCtx `lookupBound` name2
  glb
    (TyCtor MkTyCtor { name = name1, lt = lt1, args = args1 })
    (TyCtor MkTyCtor { name = name2, lt = lt2, args = args2 })
    | name1 == name2 && args1 == args2 =
      TyCtor MkTyCtor { name = name1, lt = lt1 `glb` lt2, args = args1 }
  glb
    (TyFun MkTyFun { ctx = ctx1, lt = lt1, args = args1, res = res1 })
    (TyFun MkTyFun { ctx = ctx2, lt = lt2, args = args2, res = res2 })
    | ctx1 == ctx2 && length args1 == length args2 =
      TyFun MkTyFun { ctx = ctx1, lt = lt1 `glb` lt2, args = zipWith lub args1 args2, res = res1 `glb` res2 }
  glb ty1 ty2 =
    let lt = glbAll (ty1 `ltsAt` PositivePos) `glb` glbAll (ty2 `ltsAt` PositivePos) in
    TyCtor MkTyCtor { name = "Bot", lt, args = [] }

glbAll :: Foldable f => f Lt -> Lt
glbAll = foldr glb ltFree


class MonadFresh res m where
  fresh :: m res

runFreshT :: Functor m => StateT Int m a -> m a
runFreshT = fmap fst . flip runStateT 0

instance Monad m => MonadFresh LtName (StateT Int m) where
  fresh = do
    curr <- get
    modify' (+1)
    pure $ "$l" <> show curr


infix 6 `subTyOf`
subTyOf :: (?tyCtx :: TyCtx) => MonoTy -> MonoTy -> Bool
subTyOf = curry \case
  (ty, TyCtor MkTyCtor { name = "Any", lt = lt2 }) ->
    lubAll (ty `ltsAt` PositivePos) `subLtOf` lt2

  (TyVar name1, TyVar name2) -> name1 == name2
  (TyVar name1, ty) -> (?tyCtx `lookupBound` name1) `subTyOf` ty

  ( TyCtor MkTyCtor { name = ctor1, lt = lt1, args = args1 },
    TyCtor MkTyCtor { name = ctor2, lt = lt2, args = args2 } ) ->
    ctor1 == ctor2 && lt1 `subLtOf` lt2 && args1 == args2

  ( TyFun MkTyFun { ctx = ctx1, lt = lt1, args = args1, res = res1 },
    TyFun MkTyFun { ctx = ctx2, lt = lt2, args = args2, res = res2 } ) -> and
    [ length ctx1 == length ctx2
    , and $ zipWith subTyOf ctx2 ctx1
    , length args1 == length args2
    , and $ zipWith subTyOf args2 args1
    , lt1 `subLtOf` lt2
    , res1 `subTyOf` res2
    ]

  _ -> False

subTySchemaOf :: TySchema -> TySchema -> Bool
subTySchemaOf
  MkTySchema{ ltParams = ltParams1, tyParams = tyParams1, ty = ty1 }
  MkTySchema{ ltParams = ltParams2, tyParams = tyParams2, ty = ty2 } =
  let ?tyCtx = TyCtxTy <$> tyParams2 in
  ltParams1 == ltParams2 && tyParams1 == tyParams2 && ty1 `subTyOf` ty2

subLtOf :: (?tyCtx :: TyCtx) => Lt -> Lt -> Bool
subLtOf = curry \case
  (LtMin l1 r2, lt2) -> l1 `subLtOf` lt2 && l2 `subLtOf` lt2
  (LtMin l1 r2, lt2) -> l1 `subLtOf` lt2 || l2 `subLtOf` lt2
  (lt1, lt2) -> lt1 == LtFree || lt2 == LtLocal || lt1 == lt2

eliminateLts :: Set LtName -> Lt -> Maybe PositionSign -> MonoTy -> MonoTy
eliminateLts targetNames upperBound currSign = \case
  TyVar name -> TyVar name
  TyCtor MkTyCtor { name, lt, args } -> TyCtor MkTyCtor
    { name
    , lt = approximate lt
    , args = map (rec Nothing) args
    }
  TyFun MkTyFun { ctx, lt, args, res } -> TyFun MkTyFun
    { ctx = map (rec (changeSign <$> currSign)) ctx
    , lt = approximate lt
    , args = map (rec (changeSign <$> currSign)) args
    , res = rec currSign res
    }
  where
    rec = eliminateLts targetNames upperBound

    approximateName name
      | name `Set.notMember` targetNames = ltVar name
      | otherwise = case currSign of
        Nothing -> LtStar
        Just PositivePos -> upperBound
        Just NegativePos -> ltFree

    approximate = \case
      LtLocal -> LtLocal
      LtStar -> LtStar
      LtMin l r -> lubAll $ map approximateName l

paramsToTyCtxEntry :: Bool -> Param -> TyCtxEntry
paramsToTyCtxEntry contextual MkParam { name, ty }
  | contextual = TyCtxCap MkTyCtxCap { name, monoTy = ty }
  | otherwise = TyCtxVar MkTyCtxVar { name, tySchema = emptyTySchema ty }

ensureMonoTy :: (HasCallStack, MonadError String m) => TySchema -> m MonoTy
ensureMonoTy = \case
  MkTySchema { ltParams = [], tyParams = [], ty } -> pure ty
  schema -> throwError $ "Expected mono type, got " <> show schema <> " at\n" <> prettyCallStack callStack

freeVarsOf :: HasCallStack => Expr -> Set VarName
freeVarsOf = \case
  Const _ -> Set.empty
  Plus lhs rhs -> freeVarsOf lhs <> freeVarsOf rhs
  Var name -> Set.singleton name
  TLam MkTLam { body } -> freeVarsOf body
  TApp MkTApp { lhs } -> freeVarsOf lhs
  CapCtor MkCapCtor { } -> error "Unused"
  Lam MkLam { ctxParams, params, body } ->
    freeVarsOf body \\ toSetOf (folded % #name) (ctxParams <> params)
  App MkApp { callee, ctxArgs, args } ->
    freeVarsOf callee <> foldMap freeVarsOf ctxArgs <> foldMap freeVarsOf args
  Match MkMatch { scrutinee, branches } ->
    freeVarsOf scrutinee <> flip foldMap branches \MkBranch{ varPatterns, body } ->
      freeVarsOf body \\ Set.fromList varPatterns
  Perform MkPerform { cap, args } ->
    freeVarsOf cap <> foldMap freeVarsOf args
  Handle MkHandle { capName, handler, body } ->
    capName `Set.delete` freeVarsOf body <>
    flip foldMap handler \MkHandlerEntry { paramNames, body } ->
      freeVarsOf body \\ Set.fromList ("resume" : paramNames)
  other -> error $ "unsupported free vars " <> show other

data PositionSign = PositivePos | NegativePos deriving Eq

changeSign :: PositionSign -> PositionSign
changeSign = \case PositivePos -> NegativePos; NegativePos -> PositivePos

freeLtVarsOf
  :: (HasCallStack, ?tyCtx :: TyCtx)
  => MonoTy -> Set LtName
freeLtVarsOf ty =
  let lts = ty `ltsAt` PositivePos <> ty `ltsAt` NegativePos in
  folded % _LtMin % folded `toSetOf` lts

class LifetimesAt ty where
  ltsAt
    :: (HasCallStack, ?tyCtx :: TyCtx)
    => ty -> PositionSign -> Set Lt

instance LifetimesAt ty => LifetimesAt [ty] where
  ltsAt tys expectedSign = foldMap (`ltsAt` expectedSign) tys

instance LifetimesAt TySchema where
  ltsAt MkTySchema{ ltParams, tyParams, ty } expectedSign =
    let ?tyCtx = filterVars (each % #name `toSetOf` tyParams) ?tyCtx in
    let lts :: Set Lt = ty `ltsAt` expectedSign in
    flip foldMap lts \case
      LtMin l r -> LtMin $ foldr LtMin ltParams
      LtVar v -> LtMin v
      lt -> Set.singleton lt

-- LtMin names -> Set.singleton $ LtMin (names \\ Set.fromList ltParams)

instance LifetimesAt MonoTy where
  ltsAt monoTy expectedSign = execWriter $ monoTy `goFrom` PositivePos
    where
      goFrom :: MonadWriter (Set Lt) m => MonoTy -> PositionSign -> m ()
      goFrom ty currSign = case ty of
        TyVar name ->
          runExceptT (?tyCtx `lookupBound` name) >>= \case
            Left _ -> pure () -- ignore bounds of unknown type parameters
            Right bound -> bound `goFrom` currSign
        TyCtor MkTyCtor { lt, args } -> do
          when (currSign == expectedSign) $
            tell $ Set.singleton lt
          -- Type parameters are invariant, include them in both ways.
          forM_ args (`goFrom` currSign)
          forM_ args (`goFrom` changeSign currSign)
        TyFun MkTyFun { ctx, lt, args, res } -> do
          forM_ (ctx <> args) (`goFrom` changeSign currSign)
          when (currSign == expectedSign) $
            tell $ Set.singleton lt
          res `goFrom` currSign

freeTyVarsAt :: MonoTy -> PositionSign -> Set TyName
freeTyVarsAt ty expectedSign = execWriter (ty `goAt` PositivePos)
  where
    goAt :: MonadWriter (Set TyName) m => MonoTy -> PositionSign -> m ()
    goAt ty currSign = case ty of
      TyVar name ->
        when (currSign == expectedSign) $
          tell $ Set.singleton name
      TyCtor MkTyCtor { args } -> do
        forM_ args (`goAt` currSign)
        forM_ args (`goAt` changeSign currSign)
      TyFun MkTyFun { ctx, args, res } -> do
        forM_ ctx (`goAt` changeSign currSign)
        forM_ args (`goAt` changeSign  currSign)
        res `goAt` currSign

freeTyVars :: MonoTy -> Set TyName
freeTyVars ty = ty `freeTyVarsAt` PositivePos <> ty `freeTyVarsAt` NegativePos

lookupImplicit :: MonadError String m => TyCtx -> MonoTy -> m VarName
lookupImplicit tyCtx target = go Set.empty tyCtx
  where
    go seen = let ?tyCtx = tyCtx in \case
      [] -> throwError $ "No contextual value for " <> show target
      TyCtxCap MkTyCtxCap { name, monoTy } : _ | monoTy `subTyOf` target ->
        if name `Set.notMember` seen then pure name else
          throwError $ "Variable '" <> name <> "' shadows appropriate implicit binding"
      TyCtxVar MkTyCtxVar { name } : rest -> go (Set.insert name seen) rest
      _ : rest -> go seen rest

lookupImplicits :: MonadError String m => TyCtx -> [MonoTy] -> m [VarName]
lookupImplicits tyCtx = mapM (tyCtx `lookupImplicit`)
