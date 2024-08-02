module Main (main) where

import Test.Compat (TestTree, testGroup)
import Test.Tasty (defaultMain)
import Test.Utf8 qualified

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup
    "test"
    [ Test.Utf8.testTree
    ]
