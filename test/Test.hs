{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import NaturalSort

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "properties" []

unitTests :: TestTree
unitTests = testGroup "unitTests"
  [ testCase "a is less than b" $
    "a" `compare` "b" @?= LT
  , testCase "a is naturally less than b" $
    "a" `ncompare` "b" @?= LT
  , testCase "can parse a number" $
    par number "1" @?= 1
  , testCase "can parse a non number" $
    par nonNumber "abc" @?= "abc"
  , testCase "can parse a mix" $
    par parts "1abc" @?= [N 1, T "abc"]
  , testCase "z2 is naturally less than z11" $
    "z2" `ncompare` "z11" @?= LT
  , testCase "blank is naturally less than z11" $
    "" `ncompare` "z11" @?= LT
  , testCase "natural sorting works" $
    sortBy ncompare ["2 ft 7 in", "1 ft 5 in", "10 ft 2 in", "2 ft 11 in", "7 ft 6 in"] @?= ["1 ft 5 in", "2 ft 7 in", "2 ft 11 in", "7 ft 6 in", "10 ft 2 in"]
  ]
