module Types where

import Common

import Data.Char
import Data.Data
import Data.Function
import Data.Typeable
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.List qualified as List
import Text.PrettyPrint.GenericPretty
import Optics

type TyName = String
type LtName = String
type OpName = String

-- | Lifetimes.
data Lt
  = LtLocal
  | LtFree
  | LtVar LtName
  | LtMin Lt Lt
  | LtMax Lt Lt
  | LtStar
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving Out via ShowOut Lt

ltVar :: LtName -> Lt
ltVar = LtVar

ltLocal :: Lt
ltLocal = LtLocal

ltFree :: Lt
ltFree = LtFree

-- ltMin :: Foldable f => f LtName -> Lt
-- ltMin = undefined

ltStar :: Lt
ltStar = LtStar

instance Show Lt where
  show = \case
    LtLocal -> "local"
    LtFree -> "free"
    LtStar -> "*"
    LtVar n -> n 
    LtMin l r -> show l <> "+" <> show r
    LtMax l r -> show l <> "-" <> show r

data MonoTy = TyVar TyName | TyCtor TyCtor | TyFun TyFun
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving Out via ShowOut MonoTy

instance Show MonoTy where
  show = \case
    TyVar name -> name
    TyCtor ctor -> show ctor
    TyFun fun -> show fun

data TyCtor = MkTyCtor { name :: TyName, lt :: Lt, args :: [MonoTy] }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving Out via ShowOut TyCtor

instance Show TyCtor where
  show MkTyCtor { name, lt, args } =
    let args' = if null args then "" else "<" ++ List.intercalate ", " (show <$> args) ++ ">" in
    name <> args' <> "'" <> show lt

data TyFun = MkTyFun
  { ctx :: EffRow
  , lt :: Lt
  , args :: [MonoTy]
  , res :: MonoTy
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving Out via ShowOut TyFun

instance Show TyFun where
  show MkTyFun { ctx, lt, args, res } = concat
    [ if null ctx then "" else "context(" <> List.intercalate ", " (show <$> ctx) <> ") "
    , "(" <> List.intercalate ", " (show <$> args) <> ")"
    , "'" <> show lt
    , " -> " <> show res
    ]

type EffRow = [MonoTy]

data TyParam = MkTyParam { name :: TyName, bound :: MonoTy }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving Out via ShowOut TyParam

instance Show TyParam where
  show MkTyParam { name, bound } =
    name <> " <: " <> show bound

data TySchema = MkTySchema
  { ltParams :: [LtName]
  , tyParams :: [TyParam]
  , ty :: MonoTy
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving Out via ShowOut TySchema

instance Show TySchema where
  show MkTySchema { ltParams, tyParams, ty } = concat
    [ if null ltParams && null tyParams then "" else "forall"
    , if null ltParams then "" else "[" <> List.intercalate ", " ltParams <> "]"
    , if null tyParams then "" else "<" <> List.intercalate ", " (show <$> tyParams) <> "> "
    , show ty
    ]

type EffSig = Map OpName OpSig

data OpSig = MkOpSig
  { tyParams :: [TyName]
  , args :: [MonoTy]
  , res :: MonoTy
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow OpSig

emptyTySchema :: MonoTy -> TySchema
emptyTySchema = MkTySchema [] []

class Top ty where
  top :: ty
instance Top TyCtor where
  top = tyAnyOf LtLocal
instance Top MonoTy where
  top = TyCtor top

class AnyOf ty where
  tyAnyOf :: Lt -> ty
instance AnyOf TyCtor where
  tyAnyOf lt = MkTyCtor { name = "Any", lt, args = [] }
instance AnyOf MonoTy where
  tyAnyOf = TyCtor . tyAnyOf

makePrisms ''Lt
makePrisms ''MonoTy
