module Driver where

import Syntax
import Lexer
import Types
import TypingCtx
import TypingUtils
import Typing
import Parser qualified

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Exception
import Data.Function
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Text.Parsec
import Optics

parseProg :: String -> Prog
parseProg = report . parse Parser.prog "" . tokenize
  where
    report :: Either ParseError a -> a
    report = \case
      Left err -> error $ show err
      Right res -> res

collectDecls :: Prog -> (EffCtx, TyCtx)
collectDecls prog =
  let effCtx =
        [ MkEffCtxEntry { capCtor = effName <> "K", tyParams, ops, effName }
        | EffDecl MkEffDecl { effName, tyParams, ops } <- prog
        ] in
  let tyCtx =
        [ TyCtxCtor MkTyCtxCtor
          { name = ctorName
          , ltParams
          , tyParams = [MkTyParam { name, bound = top } | name <- tyParams]
          , params
          , res = MkTyCtor
            { name = tyName
            , lt = ctorLifetime params
            , args = TyVar <$> tyParams
            }
          }
        | DataDecl MkDataDecl { tyName, tyParams, dataCtors } <- prog
        , MkDataCtor { ctorName, ltParams, params } <- dataCtors
        ] in
  (effCtx, tyCtx)
  where
    ctorLifetime params = let ?tyCtx = [] in params
      & foldMap (ltsOf . emptyTySchema)
      & foldr lub ltFree

typecheck :: EffCtx -> TyCtx -> Prog -> IO (Map String TySchema)
typecheck effCtx tyCtx prog = fold . reverse <$> flip evalStateT tyCtx do
  let ?effCtx = effCtx
  forM prog \case
    VarDecl MkVarDecl{ name, body, expectedTy } -> do
      tyCtx <- get
      let ?tyCtx = tyCtx
      (tySchema, _) <- runExceptT (runFreshT $ inferExpr body) >>= either error pure
      when (isJust expectedTy && not (tySchema `subTySchemaOf` fromJust expectedTy)) $
        liftIO $ throwIO $ userError $
          "Unexpected type for '" <> name <> "':\n" <> show tySchema <> "\n not subtype of \n" <> show (fromJust expectedTy)
      modify (TyCtxVar MkTyCtxVar { name, tySchema } :)
      pure $ Map.singleton name tySchema
    FunDecl MkFunDecl { name, ltParams, tyParams, ctxParams, params, body, resTy } -> do
      let expectedTy = MkTySchema
            { ltParams, tyParams
            , ty = TyFun MkTyFun
                { ctx = each % #ty `toListOf` ctxParams
                , lt = ltFree
                , args = each % #ty `toListOf` params
                , res = resTy
                }
            }
      let currFun = TyCtxVar MkTyCtxVar { name, tySchema = expectedTy }
      tyCtx <- get
      let ?tyCtx = currFun : tyCtx
      let expr = untyped $ TLam MkTLam { ltParams, tyParams, body = untyped $ Lam MkLam { ctxParams, params, body } }
      (tySchema, _) <- runExceptT (runFreshT (inferExpr expr)) >>= either error pure
      let actualResTy = #ty % _TyFun % #res `preview` tySchema
      unless (maybe False (`subTyOf` resTy) actualResTy) $
        liftIO $ throwIO $ userError $
          "Result type mispatch: expected " <> show resTy <> " but got " <> show actualResTy
      modify (currFun :)
      pure $ Map.singleton name tySchema
    _ -> pure Map.empty
