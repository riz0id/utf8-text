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
  -- lengthUtf8Char#,
  lengthUtf8Word8#,
  lengthUtf8Word16#,
  lengthUtf8Word32#,
) where

import GHC.Exts (Addr#, Int#, Word8#, Word16#, Word32#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

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
--       u32# = _
--    in case lengthUtf8Word32# u32# of 
--         4# -> 
--           _
--         3# -> 
--           _
--         2# -> 
--           _ 
--         _ -> 
--           _

-- Query -----------------------------------------------------------------------

unsafeIndexUtf8LengthTable# :: Int# -> Int# 
unsafeIndexUtf8LengthTable# i# = 
  let tbl# :: Addr# 
      tbl# = "\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\0\0\0\0\0\0\0\0\2\2\2\2\3\3\4\0"# 
   in GHC.ord# (GHC.indexCharOffAddr# tbl# i#)

-- | TODO 
--
-- @since 1.0.0 
-- lengthUtf8Char# :: Char# -> Int# 
-- lengthUtf8Char# c# = _

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
