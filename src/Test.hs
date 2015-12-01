{-# LANGUAGE OverloadedStrings #-}
module Test where

import Control.Monad
import Control.Monad.Trans
import Data.Either
import Data.List
import Test.Hspec
import Text.Show.Pretty
import System.Directory
import System.FilePath

import ML

main :: IO ()
main = do
  let testdir = "tests"
  ts <- filter (isSuffixOf "df") <$> getDirectoryContents testdir
  hspec $ do
    describe "Client" $ do
      forM_ (map (testdir</>) ts) $ \f -> do
        it ("syntax/"++takeFileName f) $ do
            p <- readFile f
            r <- pure $ parseProgram p
            case r of
              Left err -> error $ "Parse error: " ++ show err
              Right x -> putStrLn $ (ppShow x)
            r `shouldSatisfy`isRight


