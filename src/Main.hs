module Main where

import ML
import System.Environment
import Text.Printf
import Text.Show.Pretty

main :: IO ()
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

  printf "INPUT:\n%s\n" (unlines $ map (\(n,l) -> (show n)++ " " ++ l) ([(1::Int)..]`zip`(lines s)))
  case parseProgram s of
    Left err -> error $ "Parse error: " ++ show err
    Right x -> printf "PUTPUT:\n%s\n" (ppShow x)

