module Typing where

import Common
import Syntax
import Types
import TypingCtx
import TypingUtils

import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Foldable
import Data.Function (fix)
import Data.List qualified as List
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Map ((!?))
import Data.Map qualified as Map
import Debug.Trace
import GHC.Stack
import Optics
import Prelude hiding (lookup)

type TypingCtx m =
  ( HasCallStack, ?effCtx :: EffCtx, ?tyCtx :: TyCtx
  , MonadError String m, MonadFresh LtName m
  )

inferExpr :: TypingCtx m => Expr -> m TySchema
inferExpr = \case
  Const _ -> pure inferConst
  Var name -> inferVar name
  TLam tlam -> inferTLam tlam
  TApp tapp -> inferTApp tapp
  Lam lam -> inferLam lam
  App app -> inferApp app
  Match match -> inferMatch match
  Perform perform -> inferPerform perform
  Handle handle -> inferHandle handle
  unsupported -> error $ "Unsupported construct: " <> show unsupported

inferConst :: TySchema
inferConst = emptyTySchema $ TyCtor MkTyCtor
  { name = "Int", lt = ltFree, args = [] }

inferVar :: TypingCtx m => VarName -> m TySchema
inferVar name = ?tyCtx `lookup` name

inferTLam :: TypingCtx m => TLam -> m TySchema
inferTLam MkTLam { ltParams, tyParams, body } = do
  let ?tyCtx = fmap TyCtxTy tyParams ++ ?tyCtx
  ty <- inferExpr body >>= ensureMonoTy
  pure MkTySchema { ltParams, tyParams, ty }

inferTApp :: TypingCtx m => TApp -> m TySchema
inferTApp MkTApp { lhs, ltArgs, tyArgs }
  | null ltArgs = do
    MkTySchema { ltParams, tyParams, ty } <- inferExpr lhs
    tySubst <- mkSubst (each % #name `toListOf` tyParams) tyArgs
    paramSubst <- mkSubst (each % #name `toListOf` tyParams) $ each % #bound `toListOf` tyParams
    constraints <- collectConstraints False (paramSubst @ ty) (tySubst @ ty)
    ltSubst <- solveConstraints ltParams constraints
    pure $ emptyTySchema $ tySubst @ (ltSubst @ ty)
  | otherwise = do
    MkTySchema { ltParams, tyParams, ty } <- inferExpr lhs
    ltSubst <- mkSubst ltParams ltArgs
    checkBounds ltSubst tyParams
    tySubst <- mkSubst (each % #name `toListOf` tyParams) tyArgs
    pure $ emptyTySchema $ tySubst @ (ltSubst @ ty)
    where
      checkBounds ltSubst tyParams =
        forM_ (zip tyParams tyArgs) \(MkTyParam { name, bound }, arg) -> do
          let bound' = ltSubst @ bound
          unless (arg `subTyOf` bound') $
            throwError $ "Type argument " <> show arg <> " is not a subtype of bound "
              <> show bound' <> " of '" <> name <> "'"

inferLam :: TypingCtx m => Lam -> m TySchema
inferLam MkLam { ctxParams, params, body } = do
  checkDistinct ctxParams
  let ctxDiff = map (paramsToTyCtxEntry True) ctxParams <> map (paramsToTyCtxEntry False) params
  res <- let ?tyCtx = ctxDiff ++ ?tyCtx in inferExpr body >>= ensureMonoTy
  checkEscape res
  lt <- computeFreeLt res
  pure $ emptyTySchema $ TyFun MkTyFun
    { ctx = each % #ty `toListOf` ctxParams
    , args = each % #ty `toListOf` params
    , lt, res
    }
  where
    computeFreeLt res =
      let allParams = ctxParams <> params in
      let paramFreeTyVars = allParams & folded % #ty `foldMapOf` (`freeTyVarsAt` NegativePos) in
      let resFreeTyVars = res `freeTyVarsAt` PositivePos in
      -- Do not consider lifetimes that are already mentioned in positive positions of a function type.
      -- let ?tyCtx = filterVars (paramFreeTyVars <> resFreeTyVars) ?tyCtx in
      let boundVars = folded % #name `toSetOf` allParams in
      let lamFreeVars = freeVarsOf body \\ boundVars in
      lubAll <$> forM (Set.toList lamFreeVars) \name -> do
        tySchema :: TySchema <- ?tyCtx `lookup` name
        pure $ lubAll $ ltsOf tySchema

    checkDistinct = \case
      [] -> pure ()
      MkParam { name = name1, ty = ty1 } : rest -> do
        forM_ rest \MkParam { name = name2, ty = ty2 } ->
          case ty1 `lub` ty2 of
            TyCtor MkTyCtor { name = "Any" } -> pure ()
            _ -> throwError $ "Parameters '" <> name1 <> "' and '" <> name2
              <> " are likely to clash in implicit resolution"
        checkDistinct rest

inferApp :: TypingCtx m => App -> m TySchema
inferApp MkApp { callee, ctxArgs, args } = do
  MkTyFun { ctx = expectedCtxArgs, args = expectedArgs, res } <-
    inferExpr callee >>= ensureMonoTy >>= \case
      TyFun fun -> pure fun
      other -> throwError $ "Expected function, got " <> show other
  foundCtxArgs <- if not $ null ctxArgs then pure ctxArgs else
    fmap Var <$> ?tyCtx `lookupImplicits` expectedCtxArgs
  actualCtxArgs <- mapM (ensureMonoTy <=< inferExpr) foundCtxArgs
  actualArgs <- mapM (ensureMonoTy <=< inferExpr) args
  actualCtxArgs `checkArgsVs` expectedCtxArgs
  actualArgs `checkArgsVs` expectedArgs
  pure $ emptyTySchema res

inferMatch :: TypingCtx m => Match -> m TySchema
inferMatch MkMatch { scrutinee, branches } = do
  MkTyCtor { name = tyCtor, lt, args = tyArgs } <-
    inferExpr scrutinee >>= ensureMonoTy >>= \case
      TyCtor ctor -> pure ctor
      other -> throwError $ "Expected type ctor, got " <> show other
  let ctorCandidates = ?tyCtx `lookup` tyCtor
  unless (length branches == length ctorCandidates) $
    throwError $ "Some branches are not covered of " <> show ctorCandidates
  resTys <- forM branches \MkBranch{ ctorName, varPatterns, body } -> do
    MkTyCtxCtor { ltParams, tyParams, params } <- ctorCandidates
      & List.find @[] (\MkTyCtxCtor { name } -> name == ctorName)
      & maybe (throwError $ "Ctor " <> ctorName <> " do not have expected type") pure
    existentials <- replicateM (length ltParams) fresh
    ltSubst <- mkSubst ltParams (ltVar <$> existentials)
    tySubst <- mkSubst (each % #name `toListOf` tyParams) tyArgs
    let params' = map ((tySubst @) . (ltSubst @)) params
    unless (length varPatterns == length params) $
      throwError $ "Number of var patterns mismatch for " <> ctorName
    let paramCtx = zipWith mkCtxVar varPatterns params'
    let ?tyCtx = paramCtx ++ map (mkCtxLt lt) existentials ++ ?tyCtx
    inferExpr body >>= ensureMonoTy <&> eliminateExistentials existentials lt
  when (null resTys) $
    throwError "There should be at least one branch"
  pure $ emptyTySchema $ foldr1 lub resTys
  where
    eliminateExistentials existentials lt =
      eliminateLts (Set.fromList existentials) lt (Just PositivePos)

    mkCtxLt lt name = TyCtxLt MkTyCtxLt { name, bound = lt }

inferPerform :: TypingCtx m => Perform -> m TySchema
inferPerform MkPerform { opName, cap, tyArgs = opTyArgs, args } = do
  MkTyCtor { name = effName, args = tyArgs } <-
    inferExpr cap >>= ensureMonoTy >>= \case
      TyCtor ctor -> pure ctor
      other -> throwError $ "Expected effect type, got " <> show other
  MkEffCtxEntry { tyParams, ops } <- ?effCtx `lookup` effName
  MkOpSig { tyParams = opTyParams, args = expectedArgs, res } <- case ops !? opName of
    Nothing -> throwError $ "Effect " <> effName <> " do not include operation " <> opName
    Just op -> pure op
  subst <- mkSubst2 tyParams tyArgs opTyParams opTyArgs
  actualArgs <- mapM (ensureMonoTy <=< inferExpr) args
  actualArgs `checkArgsVs` (subst @ expectedArgs)
  pure $ emptyTySchema $ subst @ res

inferHandle :: TypingCtx m => Handle -> m TySchema
inferHandle MkHandle { capName, effTy, handler, body } = do
  let MkTyCtor { name = effName, args = effTyArgs } = effTy
  unless (#lt % only LtLocal `has` effTy) $
    throwError "Capabilities can only have local lifetime"

  let capCtx = TyCtxCap MkTyCtxCap { name = capName, monoTy = TyCtor effTy }
  resTy <- let ?tyCtx = capCtx : ?tyCtx in inferExpr body >>= ensureMonoTy
  checkEscape resTy

  MkEffCtxEntry { tyParams = effTyParams, ops } <- ?effCtx `lookup` effName
  effSubst <- mkSubst effTyParams effTyArgs
  unless (length handler == Map.size ops) $
    throwError "Wrong number of implemented operations"
  forM_ handler \MkHandlerEntry { opName, tyParams = opDefTyParams, paramNames, body } -> do
    MkOpSig { tyParams = opSigTyParams, args, res = opResTy } <-
      case ops !? opName of
        Nothing -> throwError $ "Operation " <> opName <> " is not specified for effect " <> effName
        Just sig -> pure $ effSubst @ sig
    let opTyParams = if not $ null opDefTyParams then opDefTyParams else
          replicate (length opSigTyParams) "_"
    opSubst <- mkSubst opSigTyParams (TyVar <$> opTyParams)
    let args' = opSubst @ args
    unless (length paramNames == length args') $
      throwError "Operation parameter number mismatch"
    unless (ltFree == lubAll (ltsOf args')) $
      throwError $ "Capabilities can leak through '" <> opName <> "' operation parameters"
    let tyBoundsCtx = opTyParams <&> (`mkCtxBound` tyAnyOf ltFree)
    let opParamCtx = zipWith mkCtxVar paramNames args'
    let resumeCtx = mkResume (opSubst @ opResTy) resTy
    opRetTy <- let ?tyCtx = resumeCtx : tyBoundsCtx ++ opParamCtx ++ ?tyCtx in
      inferExpr (effSubst @ body) >>= ensureMonoTy
    unless (Set.fromList opTyParams `Set.disjoint` freeTyVars opRetTy) $
      throwError $ "Operation " <> opName <> " type parameters should not leak with return"
    unless (opRetTy `subTyOf` resTy) $
      throwError $ "Operation " <> opName <> " return type " <> show opRetTy
        <> " is not a sustype of " <> show resTy
  pure $ emptyTySchema resTy
  where
    mkResume opResTy resTy = TyCtxVar MkTyCtxVar
      { name = "resume", tySchema = emptyTySchema $ TyFun MkTyFun
          { ctx = [], lt = ltFree, args = [opResTy], res = resTy }
      }
    mkCtxBound name bound = TyCtxTy MkTyParam { name, bound }

checkArgsVs :: TypingCtx m => [MonoTy] -> [MonoTy] -> m ()
checkArgsVs actualArgs expectedArgs = do
  unless (length actualArgs == length expectedArgs) $
    throwError "Arguments number mismatch"
  forM_ (zip actualArgs expectedArgs) \(actual, expected) ->
    unless (actual `subTyOf` expected) $
      throwError $ "Type mismatch: " <> show actual <> " is not a subtype of "
        <> show expected <> " from \n" <> prettyCallStack callStack

checkEscape :: TypingCtx m => MonoTy -> m ()
checkEscape res =
  -- Do not consider bounds in escape checking.
  let lts = let ?tyCtx = [] in ltsOf res in
  when (LtLocal `Set.member` lts) $
    throwError $ "Tracked value escapes via return value of type " <> show res

mkCtxVar :: TyName -> MonoTy -> TyCtxEntry
mkCtxVar name ty = TyCtxVar MkTyCtxVar { name, tySchema = emptyTySchema ty }

newtype LtConstraints = LtConstraints (Map.Map LtName LtConstraint)
data LtConstraint = LtConstraint { subOf :: [Lt], supOf :: [Lt] }

instance Show LtConstraint where
  show _ = fail "TODO"

collectConstraints :: (TypingCtx m) => Bool -> MonoTy -> MonoTy -> m LtConstraints
collectConstraints position = curry \case
  (l, TyVar r) -> do
    r <- ?tyCtx `lookupBound` r
    collectConstraints position l r
  ( TyCtor MkTyCtor { name = "Any", lt = lt1, args = _ },
    TyCtor MkTyCtor { name = _, lt = lt2, args = _ } ) ->
    pure $ sub lt1 lt2
  ( TyCtor MkTyCtor { name = ctor1, lt = lt1, args = args1 },
    TyCtor MkTyCtor { name = ctor2, lt = lt2, args = args2 } ) -> do
    -- TODO: Subtypes
    unless (ctor1 == ctor2) $
      throwError $ "Constructors don't match: " <> ctor1 <> " and " <> ctor2
    unless (length args1 == length args2) $
      throwError $ "Argument count mismatch: " <> show (length args1) <> " and " <> show (length args2)
    argConstraints <- zipWithM (collectConstraints position) args1 args2
    foldrM union (sub lt1 lt2) argConstraints
  ( TyFun MkTyFun { ctx = ctx1, lt = lt1, args = args1, res = res1 },
    TyFun MkTyFun { ctx = ctx2, lt = lt2, args = args2, res = res2 } ) -> do
    -- TODO: Subtypes
    unless (ctx1 == ctx2) $
      throwError $ "Context mismatch" <> show ctx1 <> " and " <> show ctx2
    unless (length args1 == length args2) $
      throwError $ "Argument count mismatch: " <> show (length args1) <> " and " <> show (length args2)
    -- TODO: Propagate contexts
    argConstraints <- zipWithM (collectConstraints $ not position) args1 args2
    resConstraints <- collectConstraints position res1 res2
    foldrM union (sub lt1 lt2) argConstraints >>= union resConstraints
  (_, _) -> throwError "Mismatching types"
  where
    sub l r = LtConstraints $ case l of
      (LtMin set) -> case Set.toList set of
        [l] -> Map.singleton l $ if position then LtConstraint [r] [] else LtConstraint [] [r]
        _ -> Map.empty
      _ -> Map.empty
    union (LtConstraints l) (LtConstraints r) = pure $ LtConstraints $ Map.unionWith addConstraints l r
    addConstraints (LtConstraint l1 r1) (LtConstraint l2 r2) = LtConstraint (l1 <> l2) (r1 <> r2)

solveConstraints :: (TypingCtx m) => [LtName] -> LtConstraints -> m (Subst Lt)
solveConstraints params constraints = do
  results <- mapM (getAns constraints) params
  mkSubst params results
  where
    getAns (LtConstraints constraints) name =
      maybe (pure ltFree) solveConstraint (constraints !? name)
    solveConstraint c@(LtConstraint subs sups) = do
      let super = lubAll sups
      let sub = glbAll subs
      unless (super `subLtOf` sub) $
        throwError $ "Cant solve constraint" <> show c
      pure sub