module MLSB.Printer where

import qualified Data.HashSet as HashSet

import Data.Functor.Foldable(Fix(..),Recursive(..),Corecursive(..))
import Data.List
import Data.Monoid((<>))

import MLSB.Types

printExprC :: ExprLW t -> String
printExprC = undefined {- FIXME: TODO -}
