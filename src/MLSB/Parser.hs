{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module MLSB.Parser (
    ParserError(..)
  , Lex(..)
  , tokenize
  , parseTypeC
  , parseType
  , parseExprC
  , parseExpr
  ) where

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

builtin_ids,builtin_ops,whitespace,special :: HashSet Char
builtin_ids = HashSet.fromList $ ['a'..'z']<>['A'..'Z']<>['0'..'9']<>"_"
builtin_ops = HashSet.fromList $ "*/+-<>="
whitespace = HashSet.fromList  $ " \t\n"
special = HashSet.fromList     $ ":;()[].,"

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

tok t = unCode <$> token (Cd t)
sat f = unCode <$> (satisfy $ \case { Cd c -> f c; _ -> False })
ws = fmap unWs <$> listToMaybe <$> (many (satisfy $ \case { Ws _ -> True; _ -> False }))
opt x = (Just <$> x) <|> pure Nothing

typ :: forall r . Grammar r (Prod r String Lex TypeW)
typ =
  let
    t1 f a   = Fix $ Whitespaced Nothing $ f a
    t2 f a b = Fix $ Whitespaced Nothing $ f a b
    tc c f   = Fix $ Whitespaced c $ f

    table :: [[(Holey (Prod r String Lex String), Associativity)]]
    table = [
          [([Nothing, Just (ws *> tok "->"), Nothing], RightAssoc)]
        ]

    combine [Nothing, Just oper, Nothing] exprs = foldl1 (\a b -> t2 TAppF a b) ((t1 TIdentF oper):exprs)
    combine h _ = error $ "combine: not-implemented for " <> show h

  in mdo
  -- FIXME: this version shouldn't support 1-element shapes
  tshape0 <- rule $ ws *> pure STail <?> "STail"
  tshapeI <- rule $ SConsI <$> (ws *> sat isIdent) <*> (ws *> tok "," *> tshape <|> tshape0) <?> "SConsC"
  tshapeC <- rule $ SConsC <$> (ws *> (read <$> sat (all isDigit))) <*> (ws *> tok "," *> tshape <|> tshape0) <?> "SConsI"
  tshape <- rule $ tshapeI <|> tshapeC <|> tshape0 <?> "TShape0"

  tident <- rule $ t2 TConstF <$> (ws *> (sat isIdent)) <*> (opt (ws *> tok "[" *> tshape <* ws <* tok "]")) <?> "TConst"
  texpr1 <- rule $ (ws *> tok "(") *> texpr <* (ws *> tok ")") <|> tident <?> "Type1"
  texpr <- rule $ texpr1 <|> tapp <|> tmix <?> "TExpr"
  tapp <- rule $ t2 TAppF <$> texpr <*> texpr <?> "TApp"
  tmix <- mixfixExpression table texpr1 combine

  texpr_ws <- rule $ texpr <* ws
  return texpr_ws


exprOfType :: forall r t . Grammar r (Prod r String Lex t) -> Grammar r (Prod r String Lex (ExprLW t))
exprOfType gtyp =
  let
    t1 f a = Fix $ Labeled Nothing $ Whitespaced Nothing $ f a
    t2 f a b = Fix $ Labeled Nothing $ Whitespaced Nothing $ f a b
    tc c ts f = Fix $ Labeled (listToMaybe ts) $ Whitespaced c $ f

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

  xtyp <- gtyp

  xexpr1 <- rule $ (ws *> tok "(") *> xexpr <* (ws *> tok ")") <|> xslice <|> xident <|> xconst <?> "Expr1"
  xexpr <- rule $ (ws *> tok "(") *> xexpr <* (ws *> tok ")")
              <|> xapp <|> xident <|> xconst <|> xlet <|> xlam <|> xmix <?> "Expr"
  xrational <- rule $ ConstR <$> ((fromInteger . read) <$> sat (all isDigit)) <?> "Rational" -- FIXME: accept non-int
  xpat <- rule $ Pat <$> (sat isIdent) <?> "Pattern"
  xconst <- rule $ t1 ConstF <$> (ws *> xrational)
  xident <- rule $ t1 IdentF <$> (ws *> (sat isIdent)) <?> "Ident"
  xlam <- rule $ t2 LamF <$> (ws *> xpat) <*> (ws *> tok "." *> ws *> xexpr) <?> "Lambda"
  xasgn <- rule $ (,,,) <$> ws <*> (xpat <* ws) <*> (many (tok ":" *> ws *> xtyp <* ws) <* tok "=") <*> (ws *> xexpr <* ws <* tok ";") <?> "Assign"
  xlet <- rule $ flip (foldr (\(w,p,t,e) acc -> tc w t (LetF p e acc)))
                  <$> (ws *> tok "let" *> some xasgn) <*> (ws *> tok "in" *> xexpr) <?> "Let"
  xapp <- rule $ t2 AppF <$> xexpr <*> xexpr <?> "App"
  xmix <- mixfixExpression table xexpr1 combine
  xslice <- rule $ t2 SliceF <$> xexpr1 <*> (ws *> tok "[" *> xdims <* ws <* tok "]") <?> "Slice"
  xdims <- rule $ (:) <$> xexpr <*> ((many (ws *> tok "," *> xexpr)) <|> pure []) <?> "Dims"

  xexpr_ws <- rule $ xexpr <* ws

  return xexpr_ws

expr :: forall r . Grammar r (Prod r String Lex (ExprLW TypeW))
expr = exprOfType typ

parseC :: (Show x, Eq x) => (forall r . Grammar r (Prod r String Lex x)) -> String -> Either ParserError x
parseC p str =
  let
    (res,Report{..}) = fullParses (parser p) $ tokenize str

    err = "Pos: " <> show position
       <> ": unconsumed: " <> show unconsumed
       <> ": expected one of: " <> (intercalate ", " expected)
  in
  case nub res of
    []  -> Left $ ParserError err
    [e] -> Right e
    xs  -> Left $ ParserError $ show position <> ": multiple outputs: [\n" <>
      unlines (flip map xs (\x -> show x <> ",")) <> "]"

parseTypeC :: String -> Either ParserError TypeW
parseTypeC s = parseC typ s

parseType :: String -> Either ParserError Type
parseType = either Left (Right . cata (embed . cm_next)) . parseTypeC

parseExprC :: String -> Either ParserError (ExprLW TypeW)
parseExprC s = parseC expr s

parseExpr :: String -> Either ParserError Expr
parseExpr = either Left (Right . cata (embed . cm_next . lb_next)) . parseExprC


