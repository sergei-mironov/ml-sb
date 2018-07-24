module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

import Control.Monad(when)
import Data.Maybe(fromMaybe)
import Data.Monoid ((<>))

import MLSB

parses' :: (Monad m) => String -> String -> (Expr -> Bool) -> m ()
parses' msg s chk =
  let
    err details = fail $ "Failed to parse '" <> s <> "': " <> details <> ". " <> msg
  in
  case parseExpr s of
    Right e -> when (not $ chk e) $ err $ "Unexpected: " <> show e
    Left (ParserError report) -> err report

parses :: (Monad m) => String -> m ()
parses s = parses' "Expected any Right-result" s (const True)

parsesAs :: (Monad m) => String -> Expr -> m ()
parsesAs s ans = parses' ("Expected: " <> show ans) s (==ans)

closeVal :: Val -> Val -> Bool
closeVal = eqVal (1e-5)

evals :: (Monad m) => String -> Const -> m ()
evals s ans =
  case parseExpr s of
    Right e ->
      let
        res = eval initEnv e
      in
      case (ValC ans) `closeVal` res of
        True -> return ()
        False -> fail $ "Mismatching resutls: expected "<>show ans<>", got: "<> printVal res
    Left report -> fail $ show report

main :: IO ()
main = defaultMain $
    testGroup "All" [
      testCase "Tokenize" $ do
        tokenize "asdas<>111+8" @?= ["asdas","<>","111","+","8"]
        tokenize "x (=<>=) y" @?= ["x","(","=<>=",")","y"]
        tokenize "x==y" @?= ["x","==","y"]
        tokenize "x;23+(zzz)" @?= ["x",";","23","+","(","zzz",")"]
    , testCase "Parse" $ do
        parsesAs " a " (Ident "a")
        parsesAs "a b" (App (Ident "a") (Ident "b"))
        parsesAs "let x = y ; in z" (Let (Pat "x") (Ident "y") (Ident "z"))
        parsesAs "let x = y . f ; in z" (Let (Pat "x") (Lam (Pat "y") (Ident "f")) (Ident "z"))
        parsesAs "let x = a . b . c; in z" (Let (Pat "x") (Lam (Pat "a") (Lam (Pat "b") (Ident "c"))) (Ident "z"))
        parsesAs "let x = a . b . (c  d); in z" (Let (Pat "x") (Lam (Pat "a") (Lam (Pat "b") (App (Ident "c") (Ident "d")))) (Ident "z"))
        parsesAs "10" (Const (ConstR (10)))
        parsesAs "10 + 20" (App (App (Ident "+") (Const (ConstR (10)))) (Const (ConstR (20))))
        parsesAs "x . x + x" (Lam (Pat "x") (App (App (Ident "+") (Ident "x")) (Ident "x")))
        parsesAs "let x = y ; in y + z" (Let (Pat "x") (Ident "y") (App (App (Ident "+") (Ident "y")) (Ident "z")))
        parsesAs "a b + c" (App (Ident "a") (App (App (Ident "+") (Ident "b")) (Ident "c")))
    , testCase "Eval" $ do
        evals "33" (ConstR 33)
        evals "10 + 20" (ConstR 30)
        evals "10 + (let x = 20 ;in x)" (ConstR 30)
        evals "10 + 20 * 2" (ConstR 50)
        evals "(10 + 20) * 2" (ConstR 60)
        evals "let f = x . (x + x); in (f 2)" (ConstR 4)
        return ()
    ]

