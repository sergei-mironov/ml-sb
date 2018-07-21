module Main where

import MLSB

main :: IO ()
main = do
  putStrLn $ show $ parseExpr "let x = c in s"

