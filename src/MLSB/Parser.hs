{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MLSB.Parser where

import Data.Char
import Data.List
import Data.Monoid((<>))
import Control.Applicative
import Text.Earley

import MLSB.Types

data ParserError = ParserError String
  deriving(Show, Eq, Ord)

expr :: forall r . Grammar r (Prod r String Char Expr)
expr =

  let
    tok :: Prod r String Char a -> Prod r String Char a
    tok p = many (satisfy isSpace) *> p

    term :: String -> Prod r String Char String
    term x = tok $ list x <?> x

    ident :: Prod r String Char String
    ident = tok $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum) <?> "identifier"

    num :: Prod r String Char String
    num   = tok $ some (satisfy isDigit) <?> "Number"

  in mdo

  xexpr <- rule $
        term "(" *> xexpr <* term ")"
    <|> xident
    <|> xlet
    <|> xlam
    <?> "Expr"

  xpat <- rule $ Pat <$> ident <?> "Pattern"
  xident <- rule $ Ident <$> ident <?> "Ident"
  xlam <- rule $ Lam <$> xpat <*> (term "." *> xexpr) <?> "Lambda"
  xlet <- rule $ Let <$> (term "let" *> xpat) <*> (term "=" *> xexpr) <*> (term "in" *> xexpr) <?> "Let"

  return xexpr


parseExpr :: String -> Either ParserError Expr
parseExpr str =
  let
    (res,Report{..}) = fullParses (parser expr) str

    err = "Pos: " <> show position
       <> ": unconsumed: " <> show unconsumed
       <> ": expected one of: " <> (intercalate ", " expected)
  in
  case res of
    []     -> Left $ ParserError err
    [expr] -> Right expr
    (x:xs) -> Left $ ParserError $ show position <> ": multiple outputs"


