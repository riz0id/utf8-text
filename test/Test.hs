module Main (main) where

import Test.Tasty (defaultMain)
import Test.Compat (TestTree, testGroup)

import Test.Utf8 qualified 

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup
    "Test"
    [ Test.Utf8.testTree
    ]