{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ML where

import Control.Monad

import Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Token hiding (parens, dot)

import Control.Applicative ((<$>), (<*>), (*>), (<*))

type Id = String

-- | Data type representing lambda-calculus expressions.
data Expr
  = Lam Id Expr
    -- ^ A lambda abstraction.
  | App Expr Expr
    -- ^ An expression applied to another.
  | Let Decl {-in-} Expr
    -- ^ Polymorphic let.
  deriving Eq

data Decl
  = Val Id Expr

-- | A declaration (binds a certain expression to a variable). We add this
--   abstraction on top of let so that we can write programs more easily
--   (leaving let for local declarations).
type Decl = (Id, Expr)

-- | A 'Program' is a list of declaration and an expression
--   representing what the program does. Each declaration can use
--   previous declarations only (no mutual recursion).
data Program = Program [Decl] Expr
  deriving Eq


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
  , reservedNames   = ["let", "in", "end", "val"]
  , reservedOpNames = ["|", "->", "="]
  , caseSensitive   = True
  }

lexer   = P.makeTokenParser mlDef

lid     = P.identifier lexer

llet    = P.reserved lexer "let"
lin     = P.reserved lexer "in"
lend    = P.reserved lexer "end"
lval    = P.reserved lexer "val"

ldot    = P.dot lexer
lequal  = P.reservedOp lexer "="
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

pvar     = Var <$> lid
plam     = flip (foldr Lam) <$> (many1 lid) <*> (ldot *> pexpr)
pval     = Val <$> (lid) <*> (leq *> pexpr)
plet     = Let <$> (pdecl) <*> (lin *> pexpr <* lend) 
pexpr    = plam <|> (foldl App <$> p <*> many p) where
  p = parens (plam <|> pexpr) <|> try plet <|> pvar <?> "expression"

pdecl    = (,) <$> lid <*> (lequal *> pexpr <* lsemi)
pprogram = Program <$> many (try pdecl) <*> (pexpr <* lsemi)

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

