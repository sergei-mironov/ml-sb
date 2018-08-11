{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module MLSB.Parser where

import qualified Data.HashSet as HashSet

import Control.Applicative((<$>),(<*>),(*>),(<*),(<|>),pure,many,some)
import Control.Arrow
import Data.Char
import Data.Foldable(foldr1)
import Data.Functor.Foldable(Fix(..),Recursive(..),Corecursive(..))
import Data.List
import Data.Maybe(listToMaybe)
import Data.Monoid((<>))
import Text.Earley
import Text.Earley.Mixfix
import Data.HashSet(HashSet)

import MLSB.Types

data ParserError = ParserError String
  deriving(Show, Eq, Ord)

holey :: String -> Holey Lex
holey ""       = []
holey ('_':xs) = Nothing : holey xs
holey xs       = Just (Cd i) : holey rest
  where (i, rest) = span (/= '_') xs

builtin_ids,builtin_ops,whitespace,special :: HashSet Char
builtin_ids = HashSet.fromList $ ['a'..'z']<>['A'..'Z']<>['0'..'9']<>"_"
builtin_ops = HashSet.fromList $ "*/+-<>="
whitespace = HashSet.fromList  $ " \t\n"
special = HashSet.fromList     $ ";()."

data Lex = Ws { unWs :: String } | Cd { unCode :: String }
  deriving(Show,Eq)

tokenize :: String -> [Lex]
tokenize ""        = []
tokenize (x:xs)
  | x `HashSet.member` whitespace  = Ws (x:ws_head) : tokenize ws_tail
  | x `HashSet.member` special     = Cd [x]         : tokenize xs
  | x `HashSet.member` builtin_ids = Cd (x:id_head) : tokenize id_tail
  | x `HashSet.member` builtin_ops = Cd (x:op_head) : tokenize op_tail
  | otherwise = error $ "tokenize: Invalid token: " <> (x:xs)
  where
    (ws_head, ws_tail) = break (not . flip HashSet.member whitespace) xs
    (id_head, id_tail) = break (not . flip HashSet.member builtin_ids) xs
    (op_head, op_tail) = break (not . flip HashSet.member builtin_ops) xs

isIdent :: String -> Bool
isIdent = \case
  (x:xs) -> isAlpha x && all isAlphaNum xs
  _ -> False

expr :: forall r . Grammar r (Prod r String Lex ExprW)
expr =

  let
    tok t = unCode <$> token (Cd t)
    sat f = unCode <$> (satisfy $ \case { Cd c -> f c; _ -> False })
    ws = fmap unWs <$> listToMaybe <$> (many (satisfy $ \case { Ws _ -> True; _ -> False }))
    t1 f a = Fix $ Whitespaced Nothing $ f a
    t2 f a b = Fix $ Whitespaced Nothing $ f a b
    tc c f = Fix $ Whitespaced c $ f


    table :: [[(Holey (Prod r String Lex String), Associativity)]]
    table = [
          [([Nothing, Just $ ws *> tok "+", Nothing], LeftAssoc)]
        , [([Nothing, Just $ ws *> tok "-", Nothing], LeftAssoc)]
        , [([Nothing, Just $ ws *> tok "*", Nothing], LeftAssoc)]
        , [([Nothing, Just $ ws *> tok "/", Nothing], LeftAssoc)]
        ]

    combine [Nothing, Just oper, Nothing] exprs = foldl1 (\a b -> t2 AppF a b) ((t1 IdentF oper):exprs)
    combine h _ = error $ "combine: not-implemented for " <> show h

  in mdo

  xexpr1 <- rule $ (ws *> tok "(") *> xexpr <* (ws *> tok ")") <|> xident <|> xconst <?> "Expr1"
  xexpr <- rule $ (ws *> tok "(") *> xexpr <* (ws *> tok ")")
              <|> xapp <|> xident <|> xconst <|> xlet <|> xlam <|> xmix <?> "Expr"
  xrational <- rule $ ConstR <$> ((fromInteger . read) <$> sat (all isDigit)) <?> "Rational" -- FIXME: accept non-int
  xpat <- rule $ Pat <$> (sat isIdent) <?> "Pattern"
  xconst <- rule $ t1 ConstF <$> (ws *> xrational)
  xident <- rule $ t1 IdentF <$> (ws *> (sat isIdent)) <?> "Ident"
  xlam <- rule $ t2 LamF <$> (ws *> xpat) <*> (ws *> tok "." *> ws *> xexpr) <?> "Lambda"
  xasgn <- rule $ (,,) <$> ws <*> (xpat <* ws <* tok "=") <*> (ws *> xexpr <* ws <* tok ";") <?> "Assign"
  xlet <- rule $ flip (foldr (\(w,p,e) acc -> tc w (LetF p e acc)))
                  <$> (ws *> tok "let" *> some xasgn) <*> (ws *> tok "in" *> xexpr) <?> "Let"
  xapp <- rule $ t2 AppF <$> xexpr <*> xexpr <?> "App"
  xmix <- mixfixExpression table xexpr1 combine

  xexpr_ws <- rule $ xexpr <* ws

  return xexpr_ws



parseExprC :: String -> Either ParserError ExprW
parseExprC str =
  let
    (res,Report{..}) = fullParses (parser expr) $ tokenize str

    err = "Pos: " <> show position
       <> ": unconsumed: " <> show unconsumed
       <> ": expected one of: " <> (intercalate ", " expected)
  in
  case nub res of
    []  -> Left $ ParserError err
    [e] -> Right e
    xs  -> Left $ ParserError $ show position <> ": multiple outputs: [\n" <>
      unlines (flip map xs (\x -> show x <> ",")) <> "]"


parseExpr :: String -> Either ParserError Expr
parseExpr = either Left (Right . cata (embed . cm_next)) . parseExprC


