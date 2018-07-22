{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module MLSB.Types where

type Id = String

data Pat = Pat String
  deriving(Eq,Ord,Show)

data Const =
    ConstR Rational
  | ConstS String
  deriving (Eq,Ord,Show)

eqConst :: Rational -> Const -> Const -> Bool
eqConst tol a b =
  case (a,b) of
    (ConstR af,ConstR bf) -> abs (af-bf) < tol
    (ConstS as,ConstS bs) -> as == bs
    _ -> False

-- | Data type representing lambda-calculus expressions.
data Expr =
    Const Const
    -- ^ Constant
  | Ident Id
    -- ^ Bare identifier
  | Lam Pat Expr
    -- ^ A lambda abstraction.
  | Let Pat Expr Expr
    -- ^ Let-binding
  | App Expr Expr
    -- ^ Application
  deriving (Eq,Ord,Show)

eqExpr :: Rational -> Expr -> Expr -> Bool
eqExpr tol ea eb =
  case (ea,eb) of
    (Const ca,Const cb) -> eqConst tol ca cb
    (Ident a,Ident b) -> a==b
    (Lam p1 e1,Lam p2 e2) -> p1==p2 && (eqExpr tol e1 e2)
    (Let p1 ea1 eb1,Let p2 ea2 eb2) -> p1==p2 && (eqExpr tol ea1 ea2) && (eqExpr tol eb1 eb2)
    (App ea1 ea2,App eb1 eb2) -> (eqExpr tol ea1 ea2) && (eqExpr tol eb1 eb2)
    _ -> False

data Program = Program Expr
  deriving (Show)
