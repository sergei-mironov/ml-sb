module Main where

import ML
import Text.Printf
import System.IO
import System.Environment

main = do
  a <- getArgs
  s <- case a of
    [] -> do
      getContents
    ("-":[]) ->
      getContents
    (f:[]) ->
      readFile f
    _ -> error "Expected one argument"

  printf "INPUT:\n%s\n" (unlines $ map (\(n,l) -> (show n)++ " " ++ l) ([1..]`zip`(lines s)))
  case parseProgram s of
    Left err -> error $ "Parse error: " ++ show err
    Right x -> printf "PUTPUT:\n%s\n" (show x)
  
