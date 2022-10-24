module Test.Utf8 (
  testTree,
) where

import Test.Compat (TestTree, testGroup)
import Test.Utf8.Length qualified

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Utf8"
    [ Test.Utf8.Length.testTree
    ]
