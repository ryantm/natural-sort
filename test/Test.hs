module Test where

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "properties" []

unitTests :: TestTree
unitTests = testGroup "unitTests" []
