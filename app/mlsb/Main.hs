module Main where

import System.Environment
import Text.Printf
import Text.Show.Pretty

import MLSB

main :: IO ()
main = do
  putStrLn $ show $ parseExpr "let x = c in s"

