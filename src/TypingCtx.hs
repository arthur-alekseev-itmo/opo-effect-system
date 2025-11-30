module TypingCtx where

import Common
import Types
import Syntax

import Data.Data
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import Prelude hiding (lookup)
import Control.Monad.Except
import GHC.Stack
import Text.PrettyPrint.GenericPretty
import Optics
import Control.Monad.State

type TyCtx = [TyCtxEntry]

data TyCtxEntry
  = TyCtxVar TyCtxVar
  | TyCtxCap TyCtxCap
  | TyCtxTy TyParam
  | TyCtxLt TyCtxLt
  | TyCtxCtor TyCtxCtor
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtxEntry

data TyCtxVar = MkTyCtxVar { name :: VarName, tySchema :: TySchema }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtxVar

data TyCtxCap = MkTyCtxCap { name :: VarName, monoTy :: MonoTy }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtxCap

data TyCtxLt = MkTyCtxLt { name :: LtName, bound :: Lt }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtxLt

data TyCtxCtor = MkTyCtxCtor
  { name :: CtorName
  , ltParams :: [LtName]
  , tyParams :: [TyParam]
  , params :: [MonoTy]
  , res :: TyCtor
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtxCtor

type EffCtx = [EffCtxEntry]

data EffCtxEntry = MkEffCtxEntry
  { capCtor :: CtorName
  , tyParams :: [TyName]
  , ops :: EffSig
  , effName :: TyName
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow EffCtxEntry

makePrisms ''TyCtxEntry


class Lookup ctx key result | result -> ctx key where
  lookup :: HasCallStack => ctx -> key -> result

instance MonadError String m => Lookup TyCtx VarName (m TySchema) where
  lookup tyCtx targetName = case tyCtx of
    [] -> throwError $ "Name not found '" <> targetName <> "' in ctx " <> show tyCtx <> " from\n" <> prettyCallStack callStack
    TyCtxVar MkTyCtxVar { name, tySchema } : _ | name == targetName -> pure tySchema
    TyCtxCap MkTyCtxCap { name, monoTy } : _ | name == targetName -> pure (emptyTySchema monoTy)
    TyCtxCtor MkTyCtxCtor { name, ltParams, tyParams, params, res } : _ | name == targetName ->
      pure MkTySchema
        { ltParams, tyParams
        , ty = TyFun MkTyFun { ctx = [], lt = ltFree, args = params, res = TyCtor res }
        }
    _ : rest -> rest `lookup` targetName

class LookupBound name res where
  lookupBound :: TyCtx -> name -> res

instance MonadError String m => LookupBound VarName (m MonoTy) where
  lookupBound :: TyCtx -> VarName -> m MonoTy
  lookupBound tyCtx targetName = case tyCtx of
      [] -> throwError $ "Name not found '" <> targetName <> "' in ctx " <> show tyCtx
      TyCtxTy MkTyParam { name, bound } : _ | name == targetName -> pure bound
      _ : rest -> rest `lookupBound` targetName

instance MonadError String m => LookupBound LtName (m Lt) where
  lookupBound tyCtx targetName = case tyCtx of
    [] -> throwError $ "Name not found '" <> targetName <> "' in ctx " <> show tyCtx
    TyCtxLt MkTyCtxLt { name, bound } : _ | name == targetName -> pure bound
    _ : rest -> rest `lookupBound` targetName

instance LookupBound LtName Lt where
  lookupBound tyCtx targetName = case runExcept (tyCtx `lookupBound` targetName) of
    Left _ -> ltLocal
    Right x -> x

instance LookupBound VarName MonoTy where
  lookupBound tyCtx targetName = case runExcept (tyCtx `lookupBound` targetName) of
    Left e -> error e
    Right x -> x

instance Lookup TyCtx TyName [TyCtxCtor] where
  lookup tyCtx targetName =
    [ ctor
    | TyCtxCtor ctor@MkTyCtxCtor { res = MkTyCtor { name } } <- tyCtx
    , name == targetName
    ]

instance MonadError String m => Lookup EffCtx TyName (m EffCtxEntry) where
  lookup :: MonadError String m => EffCtx -> TyName -> m EffCtxEntry
  lookup effCtx targetName =
    case List.find (\MkEffCtxEntry { effName } -> effName == targetName) effCtx of
      Nothing -> throwError $ "Effect not found " <> targetName
      Just entry -> pure entry

filterVars :: Set TyName -> TyCtx -> TyCtx
filterVars names = filter (_TyCtxTy % #name % filtered (`Set.member` names) `hasn't`)

class MonadFresh res m where
  fresh :: m res

instance Monad m => MonadFresh LtName (StateT Int m) where
  fresh = do
    curr <- get
    modify' (+1)
    pure $ "$l" <> show curr

type TypingCtx m =
  ( HasCallStack, ?effCtx :: EffCtx, ?tyCtx :: TyCtx
  , MonadError String m, MonadFresh LtName m
  )