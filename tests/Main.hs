module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, assertBool)

import Control.Monad(when)
import Data.Maybe(fromMaybe)
import Data.Monoid ((<>))

import MLSB

parses' :: (Monad m) => String -> (Expr -> Bool) -> m ()
parses' s chk =
  let
    err details = fail $ "Failed to parse '" <> s <> "': " <> details
  in
  case parseExpr s of
    Right e -> when (not $ chk e) $ err $ "unexpected result" <> show e
    Left (ParserError report) -> err report

parses :: (Monad m) => String -> m ()
parses s = parses' s (const True)

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
      testCase "Parse" $ do
        parses " a "
        parses "a b"
        parses "let x = y in z"
        parses "let x = y . f in z"
        parses "let x = a . b . c in z"
        parses "let x = a . b . (c  d) in z"
        parses "10"
        parses "10 + 20"
        return ()
    , testCase "Eval" $ do
        evals "33" (ConstR 33)
        -- evals "10 + 20" (ConstR 30)
        return ()
    ]

