module Test.Utf8.Length (
  testTree,
) where

import Data.Utf8 (lengthUtf8Word8, lengthUtf8Word16, lengthUtf8Word32)

import Hedgehog (forAll, (===))

import Test.Core (TestTree, testGroup, testCases)
import Test.Utf8.Gen qualified as Gen

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Length"
    [ testGroup
        "Word8"
        [ testCases "lengthUtf8Word8" 128 do 
            leader <- forAll Gen.leaderWord8
            lengthUtf8Word8 leader === 1
        , testCases "lengthUtf8Word16 <- Word8" 128 do 
            leader <- forAll Gen.leaderWord8
            lengthUtf8Word16 (fromIntegral leader) === 1
        , testCases "lengthUtf8Word32 <- Word8" 128 do 
            leader <- forAll Gen.leaderWord8
            lengthUtf8Word32 (fromIntegral leader) === 1
        ]
    ]
