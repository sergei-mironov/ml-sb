{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Test where

import Test.QuickCheck
import Test.QuickCheck.Text
import Test.QuickCheck.All
import Text.ParserCombinators.Parsec (parse)

import ML

tparse p str = case parse p "" str of
                Right _ -> True
                Left x -> error (show x)

prop_pexpr1 () = tparse pexpr "a"
prop_pexpr2 () = tparse pexpr "(a b)"
prop_pexpr3 () = tparse pexpr "(b b) (a b)"
prop_pexpr4 () = tparse pexpr "a . (b b) (a b)"
prop_pexpr5 () = tparse pexpr "a b . (b b) (a b)"
prop_pexpr5 () = tparse pexpr "a b . let val x = y in y (b b) (a b) end"

return []
main = $quickCheckAll
