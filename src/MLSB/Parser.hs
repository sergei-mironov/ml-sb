{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module MLSB.Parser where

import qualified Data.HashSet as HashSet

import Data.Char
import Data.List
import Data.Monoid((<>))
import Control.Applicative((<$>),(<*>),(*>),(<*),(<|>),pure,many,some)
import Text.Earley
import Data.HashSet(HashSet)

import MLSB.Types

data ParserError = ParserError String
  deriving(Show, Eq, Ord)

-- expr0 :: Grammar r (Prod r String Char Expr)
-- expr0 = mdo
--   ident     <- rule $ (V . pure . Just) <$> satisfy (not . (`HS.member` mixfixParts))
--                    <?> "identifier"
--   atom      <- rule $ ident
--                    <|> namedToken "(" *> expr <* namedToken ")"
--   normalApp <- rule $ atom
--                    <|> App <$> atom <*> some atom
--   expr      <- mixfixExpression table normalApp (App . V)
--   return expr
--   where
--     table = map (map $ first $ map $ fmap namedToken) identTable
--     mixfixParts = HS.fromList [s | xs <- identTable , (ys, _) <- xs
--                                  , Just s <- ys]
--                `mappend` HS.fromList ["(", ")"]

special :: HashSet Char
special = HashSet.fromList "(),. \n"

tokenize :: String -> [String]
tokenize ""        = []
tokenize (' ':xs)  = tokenize xs
tokenize ('\n':xs) = tokenize xs
tokenize (x:xs)
  | x `HashSet.member` special = [x] : tokenize xs
  | otherwise                  = (x:as) : tokenize bs
  where
    (as, bs) = break (`HashSet.member` special) xs

expr :: forall r . Grammar r (Prod r String String Expr)
expr =

  let
    isOp x = elem x "+-*/?"

    isIdent = \case
      (x:xs) -> isAlpha x && all isAlphaNum xs
      [] -> False

    ident :: Prod r String String String
    ident = satisfy isIdent <?> "Ident"

    op :: Prod r String String String
    op = satisfy (all isOp) <?> "Operator"

    -- FIXME: parse from float notation
    rational :: Prod r String String Rational
    rational = (fromInteger . read) <$> satisfy (all isDigit) <?> "Number"

  in mdo

  xexpr <- rule $
        namedToken "(" *> xexpr <* namedToken ")"
    <|> xapp
    <|> xop
    <|> xident
    <|> xconst
    <|> xlet
    <|> xlam
    <?> "Expr"

  xconst <- rule $ Const <$> xrational
  xrational <- rule $ ConstR <$> rational <?> "Rational"
  xpat <- rule $ Pat <$> ident <?> "Pattern"
  xident <- rule $ Ident <$> ident <?> "Ident"
  xlam <- rule $ Lam <$> xpat <*> (namedToken "." *> xexpr) <?> "Lambda"
  xlet <- rule $ Let <$> (namedToken "let" *> xpat) <*> (namedToken "=" *> xexpr) <*> (namedToken "in" *> xexpr) <?> "Let"
  xapp <- rule $ App <$> xexpr <*> xexpr <?> "App"
  xop <- rule $ (\(e1,o,e2)->App (App o e1) e2) <$>
                ((,,) <$> xexpr <*> (Ident <$> op) <*> xexpr)

  return xexpr


parseExpr :: String -> Either ParserError Expr
parseExpr str =
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


