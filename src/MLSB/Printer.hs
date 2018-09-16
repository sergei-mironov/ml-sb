{-# LANGUAGE LambdaCase #-}
module MLSB.Printer where

import qualified Data.HashSet as HashSet

import Data.Functor.Foldable(Fix(..),Recursive(..),Corecursive(..))
import Data.List
import Data.Maybe(fromMaybe,maybe)
import Data.Monoid((<>))

import MLSB.Types

printShape :: Shape -> String
printShape s = "[" <> go s where
  go STail = "]"
  go (SConsI val STail) = show val <> "]"
  go (SConsI val sh) = val <> "," <> go sh
  go (SConsC val STail) = show val <> "]"
  go (SConsC val sh) = show val <> "," <> go sh

printType :: TypeW -> String
printType = \case
  Fix (Whitespaced mws (TConstF cname cshape)) -> fromMaybe "" mws <> cname <> (maybe "" printShape cshape)
  Fix (Whitespaced mws (TIdentF iname)) -> fromMaybe "" mws <> iname
  -- Fix (Whitespaced mws (TApp pat typ)) -> fromMaybe "" mws <> iname

printExprC :: (t -> String) -> ExprLW t -> String
printExprC ptyp expr = undefined {- FIXME: TODO -}
