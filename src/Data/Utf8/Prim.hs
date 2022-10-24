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
  -- toUtf8#,

  -- * Query
  lengthUtf8Char#,
  lengthUtf8Word8#,
  lengthUtf8Word16#,
  lengthUtf8Word32#,
) where

import GHC.Exts (Addr#, Char#, Int#, Word8#, Word16#, Word32#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

charToWord32# :: Char# -> Word32# 
charToWord32# c# = GHC.wordToWord32# (GHC.int2Word# (GHC.ord# c#))

word8ToInt# :: Word8# -> Int# 
word8ToInt# u8# = GHC.word2Int# (GHC.word8ToWord# u8#)

word16ToInt# :: Word16# -> Int# 
word16ToInt# u8# = GHC.word2Int# (GHC.word16ToWord# u8#)

word32ToInt# :: Word32# -> Int# 
word32ToInt# u32# = GHC.word2Int# (GHC.word32ToWord# u32#)

--------------------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0 
-- toUtf8# :: Char# -> Word32#
-- toUtf8# c# = 
--   let u32# :: Word32# 
--       u32# = charToWord32# c#
--    in case lengthUtf8Word32# u32# GHC.-# 1# of 
--         3# -> 
--           _
--         2# -> 
--           _
--         1# -> 
--           let b1# = _ 
--               b0# = _
--            in _
--         _ -> 
--           u32#

-- Query -----------------------------------------------------------------------

unsafeIndexUtf8LengthTable# :: Int# -> Int# 
unsafeIndexUtf8LengthTable# i# = 
  let tbl# :: Addr# 
      tbl# = "\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\0\0\0\0\0\0\0\0\2\2\2\2\3\3\4\0"# 
   in GHC.ord# (GHC.indexCharOffAddr# tbl# i#)

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
