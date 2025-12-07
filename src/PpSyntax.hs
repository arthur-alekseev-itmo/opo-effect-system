module PpSyntax where

import Syntax
import Data.List qualified as List

ppTypedExpr :: TypedExpr -> String
ppTypedExpr GExpr {ty, expr} = "(" <> ppGExprInner ppTypedExpr expr <> ": " <> show ty <> ")"

ppExpr :: Expr -> String
ppExpr GExpr {expr} = ppGExprInner ppExpr expr

ppGExprInner :: (GExpr ty -> String) -> GExprInner ty -> String
ppGExprInner pp = \case
  Const i -> show i
  Plus l r -> pp l <> " + " <> pp r
  Var n -> n
  TLam tlam -> ppTLam pp tlam
  TApp tapp -> ppTApp pp tapp
  Lam lam -> ppLam pp lam
  App app -> ppApp pp app
  CapCtor ctor -> ppCapCtor pp ctor
  _ -> error "TODO"

ppTLam :: (expr -> String) -> GTLam expr -> String
ppTLam pp MkTLam { ltParams, tyParams, body } =
  pp body <> lts <> tys
  where
    tys = "<" <> List.intercalate ", " (map show tyParams) <> ">" 
    lts = "[" <> List.intercalate ", " ltParams <> "]" 

ppTApp :: (expr -> String) -> GTApp expr -> String
ppTApp pp MkTApp { lhs, ltArgs, tyArgs } =
  pp lhs <> lts <> tys
  where
    tys = "<" <> List.intercalate ", " (map show tyArgs) <> ">" 
    lts = "[" <> List.intercalate ", " (map show ltArgs) <> "]" 

ppCapCtor :: (expr -> String) -> GCapCtor expr -> String
ppCapCtor pp MkCapCtor { name, tyArgs, marker, handler } =
  error "TODO"

ppLam :: (expr -> String) -> GLam expr -> String
ppLam pp MkLam { ctxParams, params, body } =
  -- TODO: Ctx params
  "fun" <> pars <> " = " <> pp body
  where
    pars = "(" <> List.intercalate ", " (map ppParam params) <> ")" 

ppApp :: (expr -> String) -> GApp expr -> String
ppApp pp MkApp { callee, ctxArgs, args } =
  -- TODO: Ctx args
  pp callee <> ars
  where
    ars = "(" <> List.intercalate ", " (map pp args) <> ")" 

ppParam :: Param -> String
ppParam MkParam { name, ty } = name <> ": " <> show ty