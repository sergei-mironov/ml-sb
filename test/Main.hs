module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

import Control.Monad(when)
import Data.Functor.Foldable (Fix(..), Recursive(..), Corecursive(..))
import Data.Maybe(fromMaybe)
import Data.Monoid ((<>))

import MLSB

parses' :: (Monad m, Show x) =>
     (String -> Either ParserError x)
  -> String
  -> String
  -> (x -> Bool)
  -> m ()
parses' p msg s chk =
  let
    err details = fail $ "Failed to parse '" <> s <> "': " <> details <> ". " <> msg
  in
  case p s of
    Right e -> when (not $ chk e) $ err $ "Unexpected: " <> show e
    Left (ParserError report) -> err report

parsesExpr :: (Monad m) => String -> m ()
parsesExpr s = parses' parseExpr "Expected any Right-result" s (const True)

parsesExprAs :: (Monad m) => String -> Expr -> m ()
parsesExprAs s ans = parses' parseExpr ("Expected: (" <> show ans <> ") expr") s (==ans)

parsesType :: (Monad m) => String -> m ()
parsesType s = parses' parseType "Expected any Right-result" s (const True)

parsesTypeAs :: (Monad m) => String -> Type -> m ()
parsesTypeAs s ans = parses' parseType ("Expected: (" <> show ans <> ") type") s (==ans)

closeVal :: Val -> Val -> Bool
closeVal = eqVal (1e-5)

evals :: (Monad m) => String -> Const -> m ()
evals s ans =
  case parseExpr s of
    Right e ->
      let
        res = evalExpr initEnv e
      in
      case (ValC ans) `closeVal` res of
        True -> return ()
        False -> fail $ "Mismatching resutls: expected "<>show ans<>", got: "<> printVal res
    Left report -> fail $ show report

main :: IO ()
main = defaultMain $
    testGroup "All" [
      testCase "Tokenize" $ do
        tokenize "asdas<>111+8" @?= [Cd "asdas",Cd "<>",Cd "111",Cd "+",Cd "8"]
        tokenize "x (=<>=) y" @?= [Cd "x", Ws " ", Cd"(",Cd "=<>=",Cd ")",Ws " ", Cd "y"]
        tokenize "x==y" @?= [Cd "x",Cd "==",Cd "y"]
        tokenize "x;23+(zzz)" @?= [Cd "x",Cd ";",Cd "23",Cd "+",Cd "(",Cd "zzz",Cd ")"]
    , testCase "Parse types" $ do
        parsesTypeAs " a " (TIdent "a")
        parsesTypeAs "a b" (TApp (TIdent "a") (TIdent "b"))
        parsesTypeAs " ( a ) -> b" (TApp (TApp (TIdent "->") (TIdent "a")) (TIdent "b"))
    , testCase "Parse expression" $ do
        parsesExprAs " a " (Ident "a")
        parsesExprAs "a b" (App (Ident "a") (Ident "b"))
        parsesExprAs "let x = y ; in z" (Let (Pat "x") (Ident "y") (Ident "z"))
        parsesExprAs "let x = y . f ; in z" (Let (Pat "x") (Lam (Pat "y") (Ident "f")) (Ident "z"))
        parsesExprAs "let x = a . b . c; in z" (Let (Pat "x") (Lam (Pat "a") (Lam (Pat "b") (Ident "c"))) (Ident "z"))
        parsesExprAs "let x = a . b . (c  d); in z" (Let (Pat "x") (Lam (Pat "a") (Lam (Pat "b") (App (Ident "c") (Ident "d")))) (Ident "z"))
        parsesExprAs "10" (Const (ConstR (10)))
        parsesExprAs "10 + 20" (App (App (Ident "+") (Const (ConstR (10)))) (Const (ConstR (20))))
        parsesExprAs "x . x + x" (Lam (Pat "x") (App (App (Ident "+") (Ident "x")) (Ident "x")))
        parsesExprAs "let x = y ; in y + z" (Let (Pat "x") (Ident "y") (App (App (Ident "+") (Ident "y")) (Ident "z")))
        parsesExprAs "a b + c" (App (Ident "a") (App (App (Ident "+") (Ident "b")) (Ident "c")))
    , testCase "Eval" $ do
        evals "33" (ConstR 33)
        evals "10 + 20" (ConstR 30)
        evals "10 + (let x = 20 ;in x)" (ConstR 30)
        evals "10 + 20 * 2" (ConstR 50)
        evals "(10 + 20) * 2" (ConstR 60)
        evals "let f = x . (x + x); in (f 2)" (ConstR 4)
        return ()
    ]

