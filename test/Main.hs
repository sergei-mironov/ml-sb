module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

import Control.Monad(when)
import Data.Functor.Foldable (Fix(..), Recursive(..), Corecursive(..))
import Data.Maybe(fromMaybe)
import Data.Monoid ((<>))

import MLSB

parsesM' :: (Monad m, Eq x, Show x) =>
     (String -> Either ParserError x)
  -> String
  -> String
  -> (x -> m Bool)
  -> m ()
parsesM' p msg s mchk =
  let
    err details = fail $ "Failed to parse '" <> s <> "': " <> details <> ". " <> msg
  in
  case p s of
    Right e -> do
      ok <- mchk e
      when (not ok) $ err $ "Unexpected: " <> show e
    Left (ParserError report) -> err report

parses' :: (Monad m, Eq x, Show x) =>
     (String -> Either ParserError x)
  -> String
  -> String
  -> (x -> Bool)
  -> m ()
parses' p msg s1 f = parsesM' p msg s1 (return . f)

parsesSame' :: (Monad m, Eq x, Show x) =>
     (String -> Either ParserError x)
  -> String
  -> String
  -> m ()
parsesSame' p s1 s2 =
  parsesM' p "" s1 (\x1 -> do
    parsesM' p "" s2 (\x2 -> do
      return (x1 == x2))
    return True)

parsesExpr :: (Monad m) => String -> m ()
parsesExpr s = parses' parseExpr "Expected any Right-result" s (const True)

parsesExprAs :: (Monad m) => String -> Expr -> m ()
parsesExprAs s ans = parses' parseExpr ("Expected: (" <> show ans <> ") expr") s (==ans)

parsesType :: (Monad m) => String -> m ()
parsesType s = parses' parseType "Expected any Right-result" s (const True)

parsesTypeAs :: (Monad m) => String -> Type -> m ()
parsesTypeAs s ans = parses' parseType ("Expected: (" <> show ans <> ") type") s (==ans)

parsesExprSame :: (Monad m) => String -> String -> m ()
parsesExprSame s1 s2 = parsesSame' parseExpr s1 s2

parsesTypeSame :: (Monad m) => String -> String -> m ()
parsesTypeSame s1 s2 = parsesSame' parseType s1 s2

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
        let tc x = TConst x Nothing
        parsesTypeAs " a " (tc "a")
        parsesTypeAs "a b" (TApp (tc "a") (tc "b"))
        parsesTypeAs " a -> b"     (TApp (TApp (TIdent "->") (tc "a")) (tc "b"))
        parsesTypeAs " ( a ) -> b" (TApp (TApp (TIdent "->") (tc "a")) (tc "b"))
        parsesTypeAs "a -> b -> c" (TApp (TApp (TIdent "->") (tc "a")) (TApp (TApp (TIdent "->") (tc "b")) (tc "c")))
        parsesTypeSame "a -> b -> c" "a -> (b -> c)"
        parsesTypeAs " a [ 3 , 2 ]" (TConst "a" (Just (SConsC 3 (SConsC 2 STail))))
        parsesTypeAs " a [ 3 , x ]" (TConst "a" (Just (SConsC 3 (SConsI "x" STail))))
    , testCase "Parse unlabeled expression" $ do
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
        parsesExprAs "a [b + c, 10]" (Slice (Ident "a") [App (App (Ident "+") (Ident "b")) (Ident "c"),Const (ConstR (10))])
        parsesExprAs "x + y[10]"  (App (App (Ident "+") (Ident "x")) (Slice (Ident "y") [Const (ConstR (10))]))
        parsesExprAs "x y[10]" (App (Ident "x") (Slice (Ident "y") [Const (ConstR (10))]))
    , testCase "Parse labeled expression" $ do
        {- FIXME: check type label correctness -}
        parsesExprAs "let x : int = y ; in z" (Let (Pat "x") (Ident "y") (Ident "z"))
    , testCase "Eval" $ do
        evals "33" (ConstR 33)
        evals "10 + 20" (ConstR 30)
        evals "10 + (let x = 20 ;in x)" (ConstR 30)
        evals "10 + 20 * 2" (ConstR 50)
        evals "(10 + 20) * 2" (ConstR 60)
        evals "let f = x . (x + x); in (f 2)" (ConstR 4)
        return ()
    ]

