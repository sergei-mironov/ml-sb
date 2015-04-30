{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ML where

import Control.Monad

import Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Token hiding (parens, dot)

import Control.Applicative ((<$>), (<*>), (*>), (<*))

type Id = String

-- | Data type representing lambda-calculus expressions.
data Expr =
    Lam Id Expr
    -- ^ A lambda abstraction.
  | App Expr Expr
    -- ^ An expression applied to another.
  | Let Decl {-in-} Expr
    -- ^ Polymorphic let.
  | Case Expr [(Expr,Expr)]
    -- ^ Case expression
  deriving (Eq,Show)

data Decl =
    Val Id Expr
  deriving (Eq,Show)

data Program = Program [Decl]
  deriving (Eq,Show)


{-
 _
| |    _____  _____ _ __
| |   / _ \ \/ / _ \ '__|
| |__|  __/>  <  __/ |
|_____\___/_/\_\___|_|

 -}

mlDef :: LanguageDef ()
mlDef = LanguageDef
  { commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  , identStart      = letter
  , identLetter     = alphaNum <|> P.char '_'
  , opStart         = mzero
  , opLetter        = mzero
  , reservedNames   = ["let", "in", "end", "val", "case"]
  , reservedOpNames = ["|", "->", "="]
  , caseSensitive   = True
  }

lexer   = P.makeTokenParser mlDef

lid     = P.identifier lexer

llet    = P.reserved lexer "let"
lin     = P.reserved lexer "in"
lend    = P.reserved lexer "end"
lval    = P.reserved lexer "val"
lcase   = P.reserved lexer "case"

ldot    = P.dot lexer
leq  = P.reservedOp lexer "="
larr  = P.reservedOp lexer "->"
lbar  = P.reservedOp lexer "|"
lsemi   = P.semi lexer

parens  = P.parens lexer

{-
 ____
|  _ \ __ _ _ __ ___  ___ _ __
| |_) / _` | '__/ __|/ _ \ '__|
|  __/ (_| | |  \__ \  __/ |
|_|   \__,_|_|  |___/\___|_|

 -}

plam     = flip (foldr Lam) <$> (many1 lid) <*> (ldot *> pexpr)
pval     = Val <$> lid <*> (leq *> pexpr)
plet     = Let <$> (pdecl) <*> (lin *> pexpr <* lend) 
pexpr    = plam <|> (foldl App <$> p <*> many p) where
  p = parens (plam <|> pexpr) <|> try plet <|> try pcase <?> "expression"

pcase = Case <$> (lcase *> pexpr <* lin) <*> (many ((,) <$> (lbar *> pexpr <* larr) <*> (pexpr)))
pdecl    = Val <$> (lval *> lid) <*> (leq *> pexpr)
pprogram = Program <$> many (try pdecl)

parseExpr     :: String -> Either ParseError Expr
parseExpr     = parse (spaces *> pexpr <* eof) ""
parseExpr'    :: FilePath -> IO (Either ParseError Expr)
parseExpr' fn = P.parse (spaces *> pexpr <* eof) fn <$> readFile fn

parseProgram     :: String -> Either ParseError Program
parseProgram     = parse (spaces *> pprogram <* eof) ""
parseProgram'    :: FilePath -> IO (Either ParseError Program)
parseProgram' fn = parse (spaces *> pprogram <* eof) fn <$> readFile fn

readParse :: Parser a -> String -> [(a, String)]
readParse p = either (const []) (: []) . parse p' "" where
  p' = do x <- p
          State {stateInput = input} <- getParserState
          return (x, input)

instance Read Expr where
  readsPrec _ = readParse pexpr

instance Read Program where
  readsPrec _ = readParse pprogram

