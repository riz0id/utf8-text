{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Text.Utf8.Prim
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO
--
-- @since 1.0.0
module Text.Utf8.Prim
  ( -- * Index
    byte1#,
    byte2#,
    byte3#,
    byte4#,

    -- * Query
    sizeUtf8#,

    -- * Predicates 
    isContinuationByte#,
    isSurrogate#,
  )
where

import Data.Bool.Prim (Bool#)
import Data.Bool.Prim qualified as Bool
import Data.Int.Prim (Int#)

import GHC.Exts (Word8#, Word16#, Word#, Word32#)
import GHC.Exts qualified as GHC 

-- Index -----------------------------------------------------------------------

byteshift# :: Word32# -> Int# -> Word#
byteshift# x# i# = GHC.word32ToWord# (GHC.uncheckedShiftRLWord32# x# i#)

byteshiftMask# :: Word32# -> Word32# -> Int# -> Word#
byteshiftMask# x# m# = byteshift# (GHC.andWord32# x# m#)

-- | TODO
--
-- @since 1.0.0
byte1# :: Word32# -> Word8#
byte1# x# = GHC.wordToWord8# (byteshift# x# 24#)

-- | TODO
--
-- @since 1.0.0
byte2# :: Word32# -> Word8#
byte2# x# =
  let mask# :: Word32# 
      mask# = GHC.wordToWord32# 0x00FF0000##
   in GHC.wordToWord8# (byteshiftMask# x# mask# 16#)

-- | TODO
--
-- @since 1.0.0
byte3# :: Word32# -> Word8#
byte3# x# =
  let mask# :: Word32# 
      mask# = GHC.wordToWord32# 0x0000FF00##
   in GHC.wordToWord8# (byteshiftMask# x# mask# 8#)

-- | TODO
--
-- @since 1.0.0
byte4# :: Word32# -> Word8#
byte4# x# = GHC.wordToWord8# (GHC.word32ToWord# x#)

-- Query -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
foreign import ccall unsafe "utf8_size"
  sizeUtf8# :: Word8# -> Int#

-- Predicates ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
isContinuationByte# :: Word8# -> Bool# 
isContinuationByte# x# = 
  let mask# :: Word# 
      mask# = 0x80##
   in Bool.unsafeFromInt# (GHC.eqWord# mask# (GHC.and# mask# (GHC.word8ToWord# x#)))

-- | TODO
--
-- @since 1.0.0
isSurrogate# :: Word16# -> Bool# 
isSurrogate# x# = 
  let low# :: Word16# 
      low# = GHC.wordToWord16# 0xD800##
   in Bool.unsafeFromInt# (GHC.eqWord16# x# low#)