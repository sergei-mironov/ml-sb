module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, assertBool)

import Data.Maybe(fromMaybe)
import Data.Monoid ((<>))

import MLSB

main :: IO ()
main = defaultMain $
    testGroup "Main" [
      testCase "1" $ do
        Right _ <- pure $ parseExpr "let x = y in z"
        Right _ <- pure $ parseExpr "let x = y . f in z"
        Right _ <- pure $ parseExpr "let x = a . b . c in z"
        return ()
    , testCase "2" $ do
        return ()
    ]

