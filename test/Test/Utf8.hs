
module Test.Utf8 (testTree) where

import Control.Exception (evaluate)

import Data.Utf8 qualified as Utf8

import Hedgehog (forAll, (===), evalIO, annotate)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Core (TestTree, testCases, testGroup)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "utf8"
    [ testGroup
        "chr1"
        [ testCases "ord1" 255 do
            x <- forAll (Gen.word8 $ Range.constant 0 maxBound)
            r <- evalIO (evaluate (Utf8.chr1 x))
            annotate ("Utf8.chr1 x = " ++ show r)
            x === Utf8.ord1 r
        ]
    , testGroup
        "chr2"
        [ testCases "ord2" 1920 do
            x <- forAll (Gen.word8 $ Range.constant 0b1100_0000 0b1101_1111)
            y <- forAll (Gen.word8 $ Range.constant 0b1000_0000 0b1011_1111)
            r <- evalIO (evaluate (Utf8.chr2 x y))
            annotate ("Utf8.chr2 x y = " ++ show r)
            (x, y) === Utf8.ord2 r 
        ]
    , testGroup
        "chr3"
        [ testCases "ord3" 61_440 do
            x <- forAll (Gen.word8 $ Range.constant 0b1110_0000 0b1110_1111)
            y <- forAll (Gen.word8 $ Range.constant 0b1000_0000 0b1011_1111)
            z <- forAll (Gen.word8 $ Range.constant 0b1000_0000 0b1011_1111)
            r <- evalIO (evaluate (Utf8.chr3 x y z))
            annotate ("Utf8.chr3 x y z = " ++ show r)
            (x, y, z) === Utf8.ord3 r 
        ]
    , testGroup
        "chr4"
        [ testCases "ord4" 100_00 do
            x <- forAll (Gen.word8 $ Range.constant 0b1111_0000 0b1111_0111)
            y <- forAll (Gen.word8 $ Range.constant 0b1000_0000 0b1011_1111)
            z <- forAll (Gen.word8 $ Range.constant 0b1000_0000 0b1011_1111)
            w <- forAll (Gen.word8 $ Range.constant 0b1000_0000 0b1011_1111)
            r <- evalIO (evaluate (Utf8.chr4 x y z w))
            annotate ("Utf8.chr4 x y z w = " ++ show r)
            (x, y, z, w) === Utf8.ord4 r 
        ]
    , testGroup
        "leader-length"
        [ testCases "ansi" 128 do
            leader <- forAll Gen.ascii
            1 === Utf8.lengthUtf8Char leader
        , testCases "u8" 128 do
            leader <- forAll (Gen.word8 (Range.constant 0b0000_0000 0b0111_1111))
            1 === Utf8.lengthUtf8Word8 leader
        , testCases "u16" 128 do
            leader <- forAll (Gen.word8 (Range.constant 0b0000_0000 0b0111_1111))
            1 === Utf8.lengthUtf8Word16 (fromIntegral leader)
        , testCases "u32" 128 do
            leader <- forAll (Gen.word8 (Range.constant 0b0000_0000 0b0111_1111))
            1 === Utf8.lengthUtf8Word32 (fromIntegral leader)
        ]
    ]
