module Main where

import ML
import Text.Printf

main = do
  s <- getContents
  case parseProgram s of
    Left err -> error $ "Parse error: " ++ show err
    Right x -> printf "OK"
  
