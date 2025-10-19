module Syntax where

import Common
import Data.Data
import Data.Typeable
import Optics
import Text.PrettyPrint.GenericPretty
import Types

type VarName = String

type CtorName = String

data Expr
  = Const Int -- just to debug semantics
  | Plus Expr Expr -- just to debug semantics
  | Var VarName
  | TLam TLam
  | TApp TApp
  | CapCtor CapCtor
  | Lam Lam
  | App App
  | Match Match
  | Perform Perform
  | Handle Handle
  | RtHandler RtHandler
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow Expr

data TLam = MkTLam {ltParams :: [LtName], tyParams :: [TyParam], body :: Expr}
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow TLam

data TApp = MkTApp {lhs :: Expr, ltArgs :: [Lt], tyArgs :: [MonoTy]}
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow TApp

data CapCtor = MkCapCtor
  { name :: CtorName,
    tyArgs :: [MonoTy],
    marker :: Marker,
    handler :: Handler
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow CapCtor

data Lam = MkLam {ctxParams :: [Param], params :: [Param], body :: Expr}
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow Lam

data Param = MkParam {name :: VarName, ty :: MonoTy}
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow Param

data App = MkApp {callee :: Expr, ctxArgs :: [Expr], args :: [Expr]}
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow App

data Match = MkMatch {scrutinee :: Expr, branches :: [Branch]}
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow Match

data Branch = MkBranch
  { ctorName :: CtorName,
    varPatterns :: [VarName],
    body :: Expr
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow Branch

data Perform = MkPerform
  { opName :: OpName,
    cap :: Expr,
    tyArgs :: [MonoTy],
    args :: [Expr]
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow Perform

data Handle = MkHandle
  { capName :: VarName,
    effTy :: TyCtor,
    handler :: Handler,
    body :: Expr
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow Handle

type Handler = [HandlerEntry]

data HandlerEntry = MkHandlerEntry
  { opName :: OpName,
    tyParams :: [TyName],
    paramNames :: [VarName],
    body :: Expr
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow HandlerEntry

data RtHandler = MkRtHandler {marker :: Marker, body :: Expr}
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow RtHandler

-- Delimited continuation marker.
type Marker = Int

data Decl
  = DataDecl DataDecl
  | EffDecl EffDecl
  | VarDecl VarDecl
  | FunDecl FunDecl
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow Decl

data DataDecl = MkDataDecl
  { tyName :: TyName,
    tyParams :: [TyName],
    dataCtors :: [DataCtor]
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow DataDecl

data DataCtor = MkDataCtor
  { ctorName :: CtorName,
    ltParams :: [LtName],
    params :: [MonoTy]
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow DataCtor

data EffDecl = MkEffDecl
  { effName :: TyName,
    tyParams :: [TyName],
    ops :: EffSig
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow EffDecl

data VarDecl = MkVarDecl
  { name :: VarName,
    body :: Expr,
    expectedTy :: Maybe TySchema
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow VarDecl

data FunDecl = MkFunDecl
  { name :: VarName,
    ltParams :: [LtName],
    tyParams :: [TyParam],
    ctxParams :: [Param],
    params :: [Param],
    body :: Expr,
    resTy :: MonoTy
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass (Out)
  deriving (Show) via OutShow FunDecl

type Prog = [Decl]

makePrisms ''Decl
