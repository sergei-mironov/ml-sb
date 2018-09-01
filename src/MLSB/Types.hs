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

data Shape = Shape [Integer]
  deriving(Eq,Ord,Show,Read)

data Type =
    TIdent String
  | TApp Type Type
  | TTensor Type Shape
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


{-
eqExpr :: Rational -> Expr -> Expr -> Bool
eqExpr tol ea eb =
  let
    go a b = eqExpr tol a b
  in
  case (ea, eb) of
    (Const ca,Const cb) -> eqConst tol ca cb
    (Ident a,Ident b) -> a==b
    (Lam p1 e1,Lam p2 e2) -> p1==p2 && (go e1 e2)
    (Let p1 ea1 eb1,Let p2 ea2 eb2) -> p1==p2 && (go ea1 ea2) && (go eb1 eb2)
    (App ea1 ea2,App eb1 eb2) -> (go ea1 ea2) && (go eb1 eb2)
    _ -> False
-}

data Program = Program Expr
  deriving (Show,Read)

