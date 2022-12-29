{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples   #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Utf8.Prim
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Primitive operations on UTF-8 characters.
--
-- @since 1.0.0
module Data.Utf8.Prim (
  -- * ByteArray#
  indexUtf8Array#,
  readUtf8Array#,
  writeUtf8Array#,

  -- * Encoding
  ord1#,
  ord2#,
  ord3#,
  ord4#,

  -- * Decoding
  chr1#,
  chr2#,
  chr3#,
  chr4#,

  -- * Query
  lengthUtf8Char#,
  lengthUtf8Word8#,
  lengthUtf8Word16#,
  lengthUtf8Word32#,
) where

import Data.Utf8.TH (defineStaticIntArrayQ)

import GHC.Exts (Addr#, Char#, Int#, Word#, Word16#, Word32#, Word8#, State#, MutableByteArray#, Int (..), ByteArray#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

charToWord# :: Char# -> Word#
charToWord# x# = GHC.int2Word# (GHC.ord# x#)

wordToChar# :: Word# -> Char#
wordToChar# x# = GHC.chr# (GHC.word2Int# x#)

word8ToInt# :: Word8# -> Int#
word8ToInt# u8# = GHC.word2Int# (GHC.word8ToWord# u8#)

word16ToInt# :: Word16# -> Int#
word16ToInt# u8# = GHC.word2Int# (GHC.word16ToWord# u8#)

word32ToInt# :: Word32# -> Int#
word32ToInt# u32# = GHC.word2Int# (GHC.word32ToWord# u32#)

-- ByteArray# ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
indexUtf8Array# :: 
  -- | TODO: docs
  ByteArray# -> 
  -- | TODO: docs
  Int# -> 
  -- | TODO: docs
  (# Char#, Int# #)
indexUtf8Array# src# i# = 
  let !b0# = GHC.indexWord8Array# src# i# 
   in case lengthUtf8Word8# b0# GHC.-# 1# of 
        3# -> 
          let !b1# = GHC.indexWord8Array# src# (i# GHC.+# 1#) 
              !b2# = GHC.indexWord8Array# src# (i# GHC.+# 2#) 
              !b3# = GHC.indexWord8Array# src# (i# GHC.+# 3#) 
           in (# chr4# b0# b1# b2# b3#, 4# #)
        2# -> 
          let !b1# = GHC.indexWord8Array# src# (i# GHC.+# 1#) 
              !b2# = GHC.indexWord8Array# src# (i# GHC.+# 2#) 
           in (# chr3# b0# b1# b2#, 3# #)
        1# -> 
          let !b1# = GHC.indexWord8Array# src# (i# GHC.+# 1#) 
           in (# chr2# b0# b1#, 2# #)
        0# -> 
          (# chr1# b0#, 1# #)
        n# -> 
          errorWithoutStackTrace (shows 'indexUtf8Array# ": unknown UTF-8 leader: " ++ show (I# n#))

-- | TODO: docs
--
-- @since 1.0.0
readUtf8Array# :: 
  -- | TODO: docs
  MutableByteArray# s -> 
  -- | TODO: docs
  Int# -> 
  -- | TODO: docs
  State# s -> 
  -- | TODO: docs
  (# State# s, Char#, Int# #)
readUtf8Array# src# i# st0# = 
  let !(# st1#, b0# #) = GHC.readWord8Array# src# i# st0#
   in case lengthUtf8Word8# b0# GHC.-# 1# of 
        3# -> 
          let !(# st2#, b1# #) = GHC.readWord8Array# src# (i# GHC.+# 1#) st1#
              !(# st3#, b2# #) = GHC.readWord8Array# src# (i# GHC.+# 2#) st2#
              !(# st4#, b3# #) = GHC.readWord8Array# src# (i# GHC.+# 3#) st3#
           in (# st4#, chr4# b0# b1# b2# b3#, 4# #)
        2# -> 
          let !(# st2#, b1# #) = GHC.readWord8Array# src# (i# GHC.+# 1#) st1#
              !(# st3#, b2# #) = GHC.readWord8Array# src# (i# GHC.+# 2#) st2#
           in (# st3#, chr3# b0# b1# b2#, 3# #)
        1# -> 
          let !(# st2#, b1# #) = GHC.readWord8Array# src# (i# GHC.+# 1#) st1#
           in (# st2#, chr2# b0# b1#, 2# #)
        0# -> 
          (# st1#, chr1# b0#, 1# #)
        n# -> 
          errorWithoutStackTrace (shows 'readUtf8Array# ": unknown UTF-8 leader: " ++ show (I# n#))

-- | TODO: docs
--
-- @since 1.0.0
writeUtf8Array# :: 
  -- | TODO: docs
  MutableByteArray# s -> 
  -- | TODO: docs
  Int# -> 
  -- | TODO: docs
  Char# -> 
  -- | TODO: docs
  State# s -> 
  -- | TODO: docs
  (# State# s, Int# #)
writeUtf8Array# dst# i# c# st0# =
  case lengthUtf8Char# c# GHC.-# 1# of 
    3# -> 
      let !(# x#, y#, z#, w# #) = ord4# c#
          !st1# = GHC.writeWord8Array# dst# (i# GHC.+# 0#) x# st0#
          !st2# = GHC.writeWord8Array# dst# (i# GHC.+# 1#) y# st1#
          !st3# = GHC.writeWord8Array# dst# (i# GHC.+# 2#) z# st2#
          !st4# = GHC.writeWord8Array# dst# (i# GHC.+# 3#) w# st3#
       in (# st4#, 4# #)
    2# -> 
      let !(# x#, y#, z# #) = ord3# c#
          !st1# = GHC.writeWord8Array# dst# (i# GHC.+# 0#) x# st0#
          !st2# = GHC.writeWord8Array# dst# (i# GHC.+# 1#) y# st1#
          !st3# = GHC.writeWord8Array# dst# (i# GHC.+# 2#) z# st2#
       in (# st3#, 3# #)
    1# -> 
      let !(# x#, y# #) = ord2# c#
          !st1# = GHC.writeWord8Array# dst# (i# GHC.+# 0#) x# st0#
          !st2# = GHC.writeWord8Array# dst# (i# GHC.+# 1#) y# st1#
       in (# st2#, 2# #)
    0# -> 
      let !x# = ord1# c#
          !st1# = GHC.writeWord8Array# dst# (i# GHC.+# 0#) x# st0#
       in (# st1#, 1# #)
    _ -> 
      (# st0#, 0# #)

-- Encoding --------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
ord1# :: Char# -> Word8#
ord1# c# = GHC.wordToWord8# (charToWord# c#)

-- | TODO: docs
--
-- @since 1.0.0
ord2# :: Char# -> (# Word8#, Word8# #)
ord2# (charToWord# -> x#) =
  let !b0# = GHC.or# 0xc0## (GHC.and# 0xff## (GHC.uncheckedShiftRL# x# 6#))
      !b1# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 0#))
   in (# GHC.wordToWord8# b0#, GHC.wordToWord8# b1# #)

-- | TODO: docs
--
-- @since 1.0.0

ord3# :: Char# -> (# Word8#, Word8#, Word8# #)
ord3# (charToWord# -> x#) =
  let !b0# = GHC.or# 0xe0## (GHC.and# 0xff## (GHC.uncheckedShiftRL# x# 12#))
      !b1# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 6#))
      !b2# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 0#))
   in (# GHC.wordToWord8# b0#, GHC.wordToWord8# b1#, GHC.wordToWord8# b2# #)

-- | TODO: docs
--
-- @since 1.0.0
ord4# :: Char# -> (# Word8#, Word8#, Word8#, Word8# #)
ord4# (charToWord# -> x#) =
  let !b0# = GHC.or# 0xf0## (GHC.and# 0xff## (GHC.uncheckedShiftRL# x# 18#))
      !b1# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 12#))
      !b2# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 6#))
      !b3# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 0#))
   in (# GHC.wordToWord8# b0#, GHC.wordToWord8# b1#, GHC.wordToWord8# b2#, GHC.wordToWord8# b3# #)

-- ord4# :: Char# -> (# Word8#, Word8#, Word8#, Word8# #)
-- ord4# (charToWord# -> b0#) =
--   let !b1# = GHC.and# b0# 0xff3f3f3f##
--       !b2# = GHC.or#  b1# 0xf0808080##
--    in Vec4x8.unpack# (GHC.wordToWord32# b2#)

-- Decoding --------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
chr1# :: Word8# -> Char#
chr1# x# = wordToChar# (GHC.word8ToWord# x#)

-- | TODO
--
-- @since 1.0.0
chr2# :: Word8# -> Word8# -> Char#
chr2# x# y# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x3f##) 6#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 0#
   in wordToChar# (GHC.plusWord# b0# b1#)

-- | TODO
--
-- @since 1.0.0
chr3# :: Word8# -> Word8# -> Word8# -> Char#
chr3# x# y# z# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x1f##) 12#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 6#
      !b2# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# z#) 0x7f##) 0#
   in wordToChar# (GHC.plusWord# b0# (GHC.plusWord# b1# b2#))

-- | TODO
--
-- @since 1.0.0
chr4# :: Word8# -> Word8# -> Word8# -> Word8# -> Char#
chr4# x# y# z# w# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x0f##) 18#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 12#
      !b2# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# z#) 0x7f##) 6#
      !b3# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# w#) 0x7f##) 0#
   in wordToChar# (GHC.plusWord# b0# (GHC.plusWord# b1# (GHC.plusWord# b2# b3#)))

-- Query -----------------------------------------------------------------------

unsafeIndexUtf8LengthTable# :: Int# -> Int#
unsafeIndexUtf8LengthTable# i# =
  let tbl# :: Addr#
      tbl# = $(defineStaticIntArrayQ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0])
   in GHC.indexIntOffAddr# tbl# i#

-- | TODO
--
-- @since 1.0.0
lengthUtf8Char# :: Char# -> Int#
lengthUtf8Char# c# =
  let cmp0# = GHC.geChar# c# '\x80'#
      cmp1# = GHC.geChar# c# '\x800'#
      cmp2# = GHC.geChar# c# '\x10000'#
   in cmp0# GHC.+# cmp1# GHC.+# cmp2# GHC.+# 1#

-- | TODO
--
-- @since 1.0.0
lengthUtf8Word8# :: Word8# -> Int#
lengthUtf8Word8# u8# =
  let idx# :: Int#
      idx# = word8ToInt# (GHC.uncheckedShiftRLWord8# u8# 3#)
   in unsafeIndexUtf8LengthTable# idx#

-- | TODO
--
-- @since 1.0.0
lengthUtf8Word16# :: Word16# -> Int#
lengthUtf8Word16# u16# =
  let idx# :: Int#
      idx# = word16ToInt# (GHC.uncheckedShiftRLWord16# u16# 11#)
   in unsafeIndexUtf8LengthTable# idx#

-- | TODO
--
-- @since 1.0.0
lengthUtf8Word32# :: Word32# -> Int#
lengthUtf8Word32# u32# =
  let idx# :: Int#
      idx# = word32ToInt# (GHC.uncheckedShiftRLWord32# u32# 27#)
   in unsafeIndexUtf8LengthTable# idx#
