
module Test.Utf8.Gen
  ( leaderWord8,
  )
where

import Data.Word (Word8)

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

leaderWord8 :: Gen Word8
leaderWord8 = 
  let lower :: Word8 = 0b0000_0000
      upper :: Word8 = 0b0111_1111
   in Gen.word8 (Range.constant lower upper) 
