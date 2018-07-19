{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module MLSB.Types where

type Id = String

data Pat = Pat String
  deriving(Eq,Show)

-- | Data type representing lambda-calculus expressions.
data Expr =
    Ident Id
    -- ^ Bare identifier
  | Lam Pat Expr
    -- ^ A lambda abstraction.
  | Let Pat Expr Expr
    -- ^ Polymorphic let.
  | App Expr Expr
    -- ^ An expression applied to another.
  deriving (Eq,Show)

data Decl =
    Val Id Expr
  deriving (Eq,Show)

data Program = Program [Decl]
  deriving (Eq,Show)
