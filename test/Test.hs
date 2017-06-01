{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

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
  , testCase "1 number parses to N 1" $
    par number "1" @?= 1
  , testCase "z parses to [T \"z\"]" $
    parse "z" @?= [T "z"]
  , testCase "2 parses to [N 2]" $
    parse "2" @?= [N 2]
  , testCase "z2 is naturally less than z11" $
    "z2" `ncompare` "z11" @?= LT
  ]
