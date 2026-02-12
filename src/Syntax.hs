module Syntax where

import Common
import Types

import Data.Data
import Data.Typeable
import Text.PrettyPrint.GenericPretty
import Optics

type VarName = String
type CtorName = String

data Typed ty e = Typed e ty

data GExpr ty = GExpr {ty :: ty, expr :: GExprInner ty}
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GExpr ty)

data GExprInner ty
  = Const Int      -- just to debug semantics
  | Plus (GExpr ty) (GExpr ty) -- just to debug semantics
  | Var VarName
  | TLam (GTLam (GExpr ty))
  | TApp (GTApp (GExpr ty))
  | CapCtor (GCapCtor (GExpr ty))
  | Lam (GLam (GExpr ty))
  | App (GApp (GExpr ty))
  | Match (GMatch (GExpr ty))
  | Perform (GPerform (GExpr ty))
  | Handle (GHandle (GExpr ty))
  | RtHandler (GRtHandler (GExpr ty))
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GExprInner ty)

data GTLam expr = MkTLam { ltParams :: [LtName], tyParams :: [TyParam], body :: expr }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GTLam expr)

data GTApp expr = MkTApp { lhs :: expr, ltArgs :: [Lt], tyArgs :: [MonoTy] }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GTApp expr)

data GCapCtor expr = MkCapCtor
  { name :: CtorName
  , tyArgs :: [MonoTy]
  , marker :: Marker
  , handler :: GHandler expr
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GCapCtor expr)

data GLam expr = MkLam { ctxParams :: [Param], params :: [Param], body :: expr }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GLam expr)

data Param = MkParam { name :: VarName, ty :: MonoTy }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow Param

data GApp expr = MkApp { callee :: expr, ctxArgs :: [expr], args :: [expr] }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GApp expr)

data GMatch expr = MkMatch { scrutinee :: expr, branches :: [GBranch expr] }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GMatch expr)

data GBranch expr = MkBranch
  { ctorName :: CtorName
  , varPatterns :: [VarName]
  , body :: expr
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GBranch expr)

data GPerform expr = MkPerform
  { opName :: OpName
  , cap :: expr
  , tyArgs :: [MonoTy]
  , args :: [expr]
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GPerform expr)

data GHandle expr = MkHandle
  { capName :: VarName
  , effTy :: TyCtor
  , handler :: GHandler expr
  , body :: expr
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GHandle expr)

type GHandler expr = [GHandlerEntry expr]

data GHandlerEntry expr = MkHandlerEntry
  { opName :: OpName
  , tyParams :: [TyName]
  , paramNames :: [VarName]
  , body :: expr
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GHandlerEntry expr)

data GRtHandler expr = MkRtHandler { marker :: Marker, body :: expr }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GRtHandler expr)

-- Delimited continuation marker.
type Marker = Int

data GDecl expr
  = DataDecl DataDecl
  | EffDecl EffDecl
  | VarDecl (GVarDecl expr)
  | FunDecl (GFunDecl expr)
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GDecl expr)

data DataDecl = MkDataDecl
  { tyName :: TyName
  , tyParams :: [TyName]
  , dataCtors :: [DataCtor]
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow DataDecl

data DataCtor = MkDataCtor
  { ctorName :: CtorName
  , ltParams :: [LtName]
  , params :: [MonoTy]
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow DataCtor

data EffDecl = MkEffDecl
  { effName :: TyName
  , tyParams :: [TyName]
  , ops :: EffSig
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow EffDecl

data GVarDecl expr = MkVarDecl
  { name :: VarName
  , body :: expr
  , expectedTy :: Maybe TySchema
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GVarDecl expr)

data GFunDecl expr = MkFunDecl
  { name :: VarName
  , ltParams :: [LtName]
  , tyParams :: [TyParam]
  , ctxParams :: [Param]
  , params :: [Param]
  , body :: expr
  , resTy :: MonoTy
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow (GFunDecl expr)

type GProg expr = [GDecl expr]

type Expr = GExpr ()
type TypedExpr = GExpr TySchema

type TLam = GTLam Expr
type TApp = GTApp Expr
type CapCtor = GCapCtor Expr
type Lam = GLam Expr
type App = GApp Expr
type Match = GMatch Expr
type Branch = GBranch Expr
type Perform = GPerform Expr
type Handle = GHandle Expr
type Handler = GHandler Expr
type HandlerEntry = GHandlerEntry Expr
type RtHandler = GRtHandler Expr
type Decl = GDecl Expr
type VarDecl = GVarDecl Expr
type FunDecl = GFunDecl Expr
type Prog = GProg Expr

type TyTLam = GTLam TypedExpr
type TyTApp = GTApp TypedExpr
type TyCapCtor = GCapCtor TypedExpr
type TyLam = GLam TypedExpr
type TyApp = GApp TypedExpr
type TyMatch = GMatch TypedExpr
type TyBranch = GBranch TypedExpr
type TyPerform = GPerform TypedExpr
type TyHandle = GHandle TypedExpr
type TyHandler = GHandler TypedExpr
type TyHandlerEntry = GHandlerEntry TypedExpr
type TyRtHandler = GRtHandler TypedExpr
type TyDecl = GDecl TypedExpr
type TyVarDecl = GVarDecl TypedExpr
type TyFunDecl = GFunDecl TypedExpr

untyped :: GExprInner () -> Expr
untyped expr = GExpr { ty = (), expr }

typed :: TySchema -> GExprInner TySchema -> TypedExpr
typed ty expr = GExpr { ty, expr }

typedGen :: a -> GExprInner a -> GExpr a
typedGen ty expr = GExpr { ty, expr }

monoTyped :: MonoTy -> GExprInner TySchema -> TypedExpr
monoTyped ty expr = GExpr { ty = emptyTySchema ty, expr }

makePrisms ''GDecl
