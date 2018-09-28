{-# LANGUAGE LambdaCase #-}
module MLSB.Printer where

import qualified Data.HashSet as HashSet

import Data.Functor.Foldable(Fix(..),Recursive(..),Corecursive(..))
import Data.List
import Data.Maybe(fromMaybe,maybe)
import Data.Monoid((<>))

import MLSB.Types

printShape :: ShapeW -> String
printShape s = "[" <> go s <> "]" where
  go (Fix (Whitespaced _ STailF)) = ""
  go (Fix (Whitespaced _ (SConsIF val x))) = val <> "," <> go x
  go (Fix (Whitespaced _ (SConsCF val x))) = show val <> "," <> go x

printType :: TypeSW -> String
printType = \case
  Fix (Shaped cshape (Whitespaced mws (TConstF cname))) ->
    fromMaybe "" mws <> cname <> (printShape cshape)
  Fix (Shaped cshape (Whitespaced mws (TIdentF iname))) ->
    fromMaybe "" mws <> iname
  -- Fix (Whitespaced mws (TApp pat typ)) -> fromMaybe "" mws <> iname

printExprC :: (TypeW -> String) -> ExprLW -> String
printExprC ptyp expr = undefined {- FIXME: TODO -}
