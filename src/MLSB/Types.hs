{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module MLSB.Types where

import Text.Show.Deriving (deriveShow1)
import Text.Read.Deriving (deriveRead1)
import Data.Eq.Deriving (deriveEq1)
import Data.Functor.Classes (Show1(..))
import Data.Functor.Foldable (Base, Fix(..), Recursive(..), Corecursive(..))
import Data.Functor.Foldable.TH (makeBaseFunctor)

type Id = String

data Pat = Pat String
  deriving(Eq,Ord,Show,Read)

data Const =
    ConstR Rational
  | ConstS String
  deriving (Eq,Ord,Show,Read)

eqConst :: Rational -> Const -> Const -> Bool
eqConst tol a b =
  case (a,b) of
    (ConstR af,ConstR bf) -> abs (af-bf) < tol
    (ConstS as,ConstS bs) -> as == bs
    _ -> False

-- | Whitespace container should allow to keep information about comments,
-- linebreaks, etc.
data Whitespaced f a = Whitespaced { cm_get :: Maybe String, cm_next :: (f a) }
  deriving(Eq,Show,Read,Functor)

deriveEq1   ''Whitespaced
deriveShow1 ''Whitespaced
deriveRead1 ''Whitespaced

-- | Weak type annotation, as defined by user. Possibly wrong or incomoplete
data Labeled t f a = Labeled { lb_get :: Maybe t, lb_next :: f a }
  deriving(Eq,Show,Read,Functor)

deriveShow1 ''Labeled
deriveRead1 ''Labeled
deriveEq1   ''Labeled

-- | Strong type annotation, checked by the (missing) typechecker
data Typed t f a = Typed { tpd_get :: t, tpd_next :: f a }
  deriving(Eq,Show,Read,Functor)

deriveShow1 ''Typed
deriveRead1 ''Typed
deriveEq1   ''Typed

data Shape =
    STail
  | SConsI Id Shape
  -- ^ Shape variable
  | SConsC Integer Shape
  -- ^ Shape const
  deriving(Eq,Ord,Show,Read)

makeBaseFunctor ''Shape
deriveShow1 ''ShapeF
deriveRead1 ''ShapeF
deriveEq1   ''ShapeF

data Type =
    TConst String (Maybe Shape)
  -- ^ a, b, c[3x4], d[3xN], (->)
  | TIdent String
  | TApp Type Type
  | TLam Pat Type
  deriving(Eq,Ord,Show,Read)

makeBaseFunctor ''Type
deriveShow1 ''TypeF
deriveRead1 ''TypeF
deriveEq1   ''TypeF

type Type1 = Fix TypeF
type TypeW = Fix (Whitespaced TypeF)

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
  | Slice Expr [Expr]
    -- ^ Tensor-like slice
  deriving (Eq,Ord,Show,Read)

makeBaseFunctor ''Expr
deriveShow1 ''ExprF
deriveRead1 ''ExprF
deriveEq1   ''ExprF


type Expr1 = Fix ExprF
type ExprTW t = Fix (Typed t (Whitespaced ExprF))
type ExprLW t = Fix (Labeled t (Whitespaced ExprF))

type instance Base (Whitespaced ExprF _) = ExprF
type instance Base (Labeled _ _ (Whitespaced ExprF _)) = ExprF
type instance Base (Whitespaced TypeF _) = TypeF
type instance Base (Typed _ _ (Whitespaced ExprF _)) = ExprF

data Program = Program Expr
  deriving (Show,Read)

