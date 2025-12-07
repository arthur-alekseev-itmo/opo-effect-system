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
import Data.Maybe (fromMaybe)
import TypingConstraints
import Data.Coerce
import Foreign.C (throwErrno)

type Inferred = (TySchema, TypedExpr)

withType :: (Monad m) => TySchema -> TypedExpr -> m Inferred
withType ty expr = pure (ty, expr)

ensureMonoTyInf :: MonadError String m => Inferred -> m (MonoTy, TypedExpr)
ensureMonoTyInf (ty, expr) = do
  ty <- ensureMonoTy ty
  pure (ty, expr)

inferExpr :: TypingCtx m => Expr -> m Inferred
inferExpr (GExpr { expr }) = case expr of
  Const i -> inferConst i
  Var name -> inferVar name
  TLam tlam -> inferTLam tlam
  TApp tapp -> inferTApp tapp
  Lam lam -> inferLam lam
  App app -> inferApp app
  Match match -> inferMatch match
  Perform perform -> inferPerform perform
  Handle handle -> inferHandle handle
  unsupported -> error $ "Unsupported construct: " <> show unsupported

inferConst :: (TypingCtx m) => Int -> m Inferred
inferConst i =
  withType (emptyTySchema int) $ monoTyped int $ Const i
  where int = TyCtor MkTyCtor { name = "Int", lt = ltFree, args = [] }

inferVar :: TypingCtx m => VarName -> m Inferred
inferVar name = do
  ty <- ?tyCtx `lookup` name
  withType ty $ typed ty $ Var name

inferTLam :: TypingCtx m => TLam -> m Inferred
inferTLam MkTLam { ltParams, tyParams, body } = do
  let ?tyCtx = fmap TyCtxTy tyParams ++ ?tyCtx
  (ty, typedBody) <- inferExpr body >>= ensureMonoTyInf
  let schema = (MkTySchema { ltParams, tyParams, ty })
  let expr = typed schema $ TLam $ MkTLam { ltParams, tyParams, body = typedBody }
  withType schema expr

inferTApp :: TypingCtx m => TApp -> m Inferred
inferTApp MkTApp { lhs, ltArgs, tyArgs } = do
  (MkTySchema { ltParams, tyParams, ty = lhsTy }, lhsExpr) <- inferExpr lhs
  let expr = monoTyped lhsTy $ TApp $ MkTApp { lhs = lhsExpr, ltArgs, tyArgs }
  ltSubst <- mkSubst ltParams ltArgs
  tySubst <- mkSubst (each % #name `toListOf` tyParams) tyArgs
  checkBounds ltSubst tyParams
  let ty = emptyTySchema $ tySubst @ (ltSubst @ lhsTy)
  withType ty expr
  where
    checkBounds ltSubst tyParams =
      forM_ (zip tyParams tyArgs) \(MkTyParam { name, bound }, arg) -> do
        let bound' = ltSubst @ bound
        unless (arg `subTyOf` bound') $
          throwError $ "Type argument " <> show arg <> " is not a subtype of bound "
            <> show bound' <> " of '" <> name <> "'"

inferLam :: TypingCtx m => Lam -> m Inferred
inferLam MkLam { ctxParams, params, body } = do
  checkDistinct ctxParams
  let ctxDiff = map (paramsToTyCtxEntry True) ctxParams <> map (paramsToTyCtxEntry False) params
  (res, bodyExpr) <- let ?tyCtx = ctxDiff ++ ?tyCtx in inferExpr body >>= ensureMonoTyInf
  checkEscape res
  lt <- computeFreeLt res
  let monoTy = TyFun MkTyFun
        { ctx = each % #ty `toListOf` ctxParams
        , args = each % #ty `toListOf` params
        , lt
        , res
        }
  let ty = emptyTySchema monoTy
  let expr = typed ty $ Lam $ MkLam { ctxParams, params, body = bodyExpr }
  withType ty expr
  where
    computeFreeLt res =
      let allParams = ctxParams <> params in
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

inferApp :: TypingCtx m => App -> m Inferred
-- TODO: If lt/ty args are given check them against inferred!! #AA
inferApp MkApp { callee = GExpr { expr = TApp MkTApp { lhs, ltArgs = [], tyArgs = []} }, ctxArgs, args } = do
  (lhsSchema@MkTySchema { ltParams, tyParams, ty }, lhsExpr) <- inferExpr lhs
  MkTyFun { ctx = expectedCtxArgs, args = params, res } <-
    case ty of
      TyFun fun -> pure fun
      other -> throwError $ "Expected function, got " <> show other
  -- TODO: Ctx args #AA
  (inferredArgTypes, inferredArgs) <- mapAndUnzipM (inferExpr >=> ensureMonoTyInf) args
  constraints <- zipWithM (collectConstraints PositivePos) params inferredArgTypes <&> mconcat
  let CombinedConstraints { tyConstraints, ltConstraints } = constraints

  -- TODO: Check bounds of parameters, add them to constraints maybe
  tyArgs <- mapM (solveFor tyConstraints) (each % #name `toListOf` tyParams)
  ltArgs <- mapM (solveFor ltConstraints) ltParams

  ltSubst <- mkSubst ltParams ltArgs
  tySubst <- mkSubst (each % #name `toListOf` tyParams) tyArgs
  let deducedRes = ltSubst @ (tySubst @ res)

  -- TODO: Dicuss calling infer tapp for bounds check
  foundCtxArgs <-
    if not $ null ctxArgs then pure ctxArgs
    else
      let names = ?tyCtx `lookupImplicits` expectedCtxArgs in
      fmap (untyped . Var ) <$> names

  let typedCallee = typed lhsSchema $ TApp MkTApp { lhs = lhsExpr, ltArgs, tyArgs }
  -- TODO: Context arguments! #AA
  let typedExpr = monoTyped deducedRes $ App MkApp { callee = typedCallee, ctxArgs = [], args = inferredArgs }

  actualCtxArgs <- mapM (ensureMonoTyInf <=< inferExpr) foundCtxArgs
  (fst <$> actualCtxArgs) `checkArgsVs` expectedCtxArgs
  withType (emptyTySchema deducedRes) typedExpr

inferApp MkApp { callee, ctxArgs, args } = do
  (MkTyFun { ctx = expectedCtxArgs, args = expectedArgs, res }, calleeExpr) <-
    inferExpr callee >>= ensureMonoTyInf >>= \case
      (TyFun fun, expr) -> pure (fun, expr)
      other -> throwError $ "Expected function, got " <> show other
  foundCtxArgs <- if not $ null ctxArgs then pure ctxArgs else
    fmap (untyped . Var ) <$> ?tyCtx `lookupImplicits` expectedCtxArgs
  (actualCtxArgs, actualCtxArgExprs) <- mapAndUnzipM (ensureMonoTyInf <=< inferExpr) foundCtxArgs
  (actualArgs, actualArgExprs) <- mapAndUnzipM (ensureMonoTyInf <=< inferExpr) args
  actualCtxArgs `checkArgsVs` expectedCtxArgs
  actualArgs `checkArgsVs` expectedArgs
  let ty = emptyTySchema res
  let expr = typed ty $ App MkApp { callee = calleeExpr, ctxArgs = actualCtxArgExprs, args = actualArgExprs}
  withType ty expr

inferMatch :: TypingCtx m => Match -> m Inferred
inferMatch MkMatch { scrutinee, branches } = do
  (MkTyCtor { name = tyCtor, lt, args = tyArgs }, scrutineeExpr) <-
    inferExpr scrutinee >>= ensureMonoTyInf >>= \case
      (TyCtor ctor, expr) -> pure (ctor, expr)
      other -> throwError $ "Expected type ctor, got " <> show other
  let ctorCandidates = ?tyCtx `lookup` tyCtor
  unless (length branches == length ctorCandidates) $
    throwError $ "Some branches are not covered of " <> show ctorCandidates
  (resTys, resExprs) <- unzip <$> forM branches \MkBranch{ ctorName, varPatterns, body } -> do
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
    (ty, bodyExpr) <- inferExpr body >>= ensureMonoTyInf
    let expr = MkBranch{ ctorName, varPatterns, body = bodyExpr }
    pure (eliminateExistentials existentials lt ty, expr)
    
  when (null resTys) $
    throwError "There should be at least one branch"
  let ty = foldr1 lub resTys
  let expr = monoTyped ty $ Match MkMatch { scrutinee = scrutineeExpr, branches = resExprs }
  withType (emptyTySchema ty) expr
  where
    eliminateExistentials existentials lt =
      eliminateLts (Set.fromList existentials) lt (Just PositivePos)

    mkCtxLt lt name = TyCtxLt MkTyCtxLt { name, bound = lt }

inferPerform :: TypingCtx m => Perform -> m Inferred
inferPerform MkPerform { opName, cap, tyArgs = opTyArgs, args } = do
  (MkTyCtor { name = effName, args = tyArgs }, capExpr) <-
    inferExpr cap >>= ensureMonoTyInf >>= \case
      (TyCtor ctor, expr) -> pure (ctor, expr)
      other -> throwError $ "Expected effect type, got " <> show other
  MkEffCtxEntry { tyParams, ops } <- ?effCtx `lookup` effName
  MkOpSig { tyParams = opTyParams, args = expectedArgs, res } <- case ops !? opName of
    Nothing -> throwError $ "Effect " <> effName <> " do not include operation " <> opName
    Just op -> pure op
  subst <- mkSubst2 tyParams tyArgs opTyParams opTyArgs
  (actualArgs, actualArgsExprs) <- mapAndUnzipM (ensureMonoTyInf <=< inferExpr) args
  actualArgs `checkArgsVs` (subst @ expectedArgs)
  let ty = emptyTySchema $ subst @ res
  let expr = typed ty $ Perform MkPerform { opName, cap = capExpr, tyArgs = opTyArgs, args = actualArgsExprs }
  withType ty expr

inferHandle :: TypingCtx m => Handle -> m Inferred
inferHandle MkHandle { capName, effTy, handler, body } = do
  undefined
  -- let MkTyCtor { name = effName, args = effTyArgs } = effTy
  -- unless (#lt % only LtLocal `has` effTy) $
  --   throwError "Capabilities can only have local lifetime"

  -- let capCtx = TyCtxCap MkTyCtxCap { name = capName, monoTy = TyCtor effTy }
  -- (resTy, resExpr) <- let ?tyCtx = capCtx : ?tyCtx in inferExpr body >>= ensureMonoTyInf
  -- checkEscape resTy

  -- MkEffCtxEntry { tyParams = effTyParams, ops } <- ?effCtx `lookup` effName
  -- effSubst <- mkSubst effTyParams effTyArgs
  -- unless (length handler == Map.size ops) $
  --   throwError "Wrong number of implemented operations"
  -- forM_ handler \MkHandlerEntry { opName, tyParams = opDefTyParams, paramNames, body } -> do
  --   MkOpSig { tyParams = opSigTyParams, args, res = opResTy } <-
  --     case ops !? opName of
  --       Nothing -> throwError $ "Operation " <> opName <> " is not specified for effect " <> effName
  --       Just sig -> pure $ effSubst @ sig
  --   let opTyParams = if not $ null opDefTyParams then opDefTyParams else
  --         replicate (length opSigTyParams) "_"
  --   opSubst <- mkSubst opSigTyParams (TyVar <$> opTyParams)
  --   let args' = opSubst @ args
  --   unless (length paramNames == length args') $
  --     throwError "Operation parameter number mismatch"
  --   unless (ltFree == lubAll (ltsOf args')) $
  --     throwError $ "Capabilities can leak through '" <> opName <> "' operation parameters"
  --   let tyBoundsCtx = opTyParams <&> (`mkCtxBound` tyAnyOf ltFree)
  --   let opParamCtx = zipWith mkCtxVar paramNames args'
  --   let resumeCtx = mkResume (opSubst @ opResTy) resTy
  --   let body = undefined
  --   (opRetTy, opRetExpr) <- let ?tyCtx = resumeCtx : tyBoundsCtx ++ opParamCtx ++ ?tyCtx in
  --     inferExpr (effSubst @ body) >>= ensureMonoTyInf
  --   unless (Set.fromList opTyParams `Set.disjoint` freeTyVars opRetTy) $
  --     throwError $ "Operation " <> opName <> " type parameters should not leak with return"
  --   unless (opRetTy `subTyOf` resTy) $
  --     throwError $ "Operation " <> opName <> " return type " <> show opRetTy
  --       <> " is not a sustype of " <> show resTy
  -- -- SUPER TODO: #AA
  -- let expr = undefined 
  -- -- Handle MkHandle { capName, effTy, handler, body }
  -- withType (emptyTySchema resTy) expr
  -- where
  --   mkResume opResTy resTy = TyCtxVar MkTyCtxVar
  --     { name = "resume", tySchema = emptyTySchema $ TyFun MkTyFun
  --         { ctx = [], lt = ltFree, args = [opResTy], res = resTy }
  --     }
  --   mkCtxBound name bound = TyCtxTy MkTyParam { name, bound }

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
