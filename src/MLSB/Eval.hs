module MLSB.Eval where

import qualified Data.Map as Map

import Data.Functor.Foldable (Fix(..), Recursive(..), Corecursive(..))
import Data.Monoid((<>))
import Data.Map (Map, (!))

import MLSB.Types

data Val =
    ValC Const
  | ValFn (Val -> Val)

printVal :: Val -> String
printVal val =
  case val of
    ValC c  -> "ValC "<>show c
    ValFn _ -> "ValFn <func>"

eqVal :: Rational -> Val -> Val -> Bool
eqVal tol a b =
  case (a,b) of
    (ValC c1, ValC c2) -> eqConst tol c1 c2
    _ -> error $ "eqVal: can't compare non-constant values: " <> printVal a <> ", " <> printVal b

type Env = Map String Val

evalExpr :: Env -> Expr -> Val
evalExpr env expr =
  case expr of
    Const c -> ValC c
    Ident i ->
      case Map.lookup i env of
        Just val -> val
        Nothing -> error $ "Invalid identifier '"<>i<>"'"
    Lam (Pat p) elam -> ValFn $ \v -> evalExpr (Map.insert p v env) elam
    Let (Pat p) elet ein -> evalExpr (Map.insert p (evalExpr env elet) env) ein
    App elam earg ->
      case evalExpr env elam of
        ValFn f -> f (evalExpr env earg)
        val -> error "Unable to apply to non-lambda"
    Slice _ _ -> error "Slicing is not implemented"

evalExpr1 :: Env -> Expr1 -> Val
evalExpr1 env = evalExpr env . cata embed

emptyEnv :: Env
emptyEnv = Map.empty


initEnv :: Env
initEnv = Map.fromList $
  let
    arithm nm op =
      ValFn $ \a -> ValFn $ \b ->
        case (a,b) of
          (ValC (ConstR af), ValC (ConstR bf)) -> ValC $ ConstR $ af`op`bf
          _ -> error $ "Invalid operands for ("<>nm<>")"
  in [
    ("+", arithm "+" (+))
  , ("-", arithm "-" (-))
  , ("*", arithm "*" (*))
  , ("/", arithm "/" (/))
  , ("<>",
      ValFn $ \a -> ValFn $ \b ->
        case (a,b) of
          (ValC (ConstS as), ValC (ConstS bs)) -> ValC $ ConstS $ as<>bs
          _ -> error $ "Invalid operands for ("<>"<>"<>")"
    )
  ]
