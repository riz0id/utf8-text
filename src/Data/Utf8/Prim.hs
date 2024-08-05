{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
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
-- TODO: docs
--
-- @since 1.0.0
module Data.Utf8.Prim (
    sizeofLeaderUtf8#
  , chr1#
  , chr2#
  , chr3#
  , chr4#
  , indexByteArrayAsUtf8#
) where

import GHC.Exts (ByteArray#, Char#, Int#, Word#, Word8#, Addr#)
import GHC.Exts qualified as GHC

import Data.Utf8.TH (defineUtf8LeaderLengthTable)

--------------------------------------------------------------------------------

wordToChar# :: Word# -> Char#
wordToChar# x# = GHC.chr# (GHC.word2Int# x#)

--------------------------------------------------------------------------------

-- | Given a valid UTF-8 leader byte, obtain the number of UTF-8 code units
-- used to encode the UTF-8 character.
--
-- >>> sizeofLeaderUtf8 0x61 -- The UTF-8 representation of 'a'
-- 1
--
-- @since 1.0.0
sizeofLeaderUtf8# :: Word8# -> Int#
sizeofLeaderUtf8# leader# =
  let !i# = GHC.word2Int# (GHC.word8ToWord# (GHC.uncheckedShiftRLWord8# leader# 3#))
      !n# = GHC.indexWord8OffAddr# utf8LeaderLengthTable# i#
   in GHC.word2Int# (GHC.word8ToWord# n#)
  where
    utf8LeaderLengthTable# :: Addr#
    utf8LeaderLengthTable# = $defineUtf8LeaderLengthTable

-- | Decode a single UTF-8 code unit as a character.
--
-- @since 1.0.0
chr1# :: Word8# -> Char#
chr1# x# = wordToChar# (GHC.word8ToWord# x#)

-- | Decode two UTF-8 code units as a character.
--
-- @since 1.0.0
chr2# :: Word8# -> Word8# -> Char#
chr2# x# y# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x3f##) 6#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1#)

-- | Decode three UTF-8 code units as a character.
--
-- @since 1.0.0
chr3# :: Word8# -> Word8# -> Word8# -> Char#
chr3# x# y# z# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x1f##) 12#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 6#
      !b2# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# z#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1# `GHC.or#` b2#)

-- | Decode four UTF-8 code units as a character.
--
-- @since 1.0.0
chr4# :: Word8# -> Word8# -> Word8# -> Word8# -> Char#
chr4# x# y# z# w# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x0f##) 18#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 12#
      !b2# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# z#) 0x7f##) 6#
      !b3# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# w#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1# `GHC.or#` b2# `GHC.or#` b3#)

-- | TODO: docs
--
-- @since 1.0.0
indexByteArrayAsUtf8# ::
  -- | TODO: docs
  ByteArray# ->
  -- | TODO: docs
  Int# ->
  -- | TODO: docs
  (# Char# , Int# #)
indexByteArrayAsUtf8# bxs# i# = do
  let !u1# = GHC.indexWord8Array# bxs# i#
   in case sizeofLeaderUtf8# u1# of
        1# ->
          (# chr1# u1#, 1# #)
        2# -> do
          let !u2# = GHC.indexWord8Array# bxs# (1# GHC.+# i#)
           in (# chr2# u1# u2#, 2# #)
        3# -> do
          let !u2# = GHC.indexWord8Array# bxs# (1# GHC.+# i#)
              !u3# = GHC.indexWord8Array# bxs# (2# GHC.+# i#)
           in (# chr3# u1# u2# u3#, 3# #)
        4# -> do
          let !u2# = GHC.indexWord8Array# bxs# (1# GHC.+# i#)
              !u3# = GHC.indexWord8Array# bxs# (2# GHC.+# i#)
              !u4# = GHC.indexWord8Array# bxs# (3# GHC.+# i#)
           in (# chr4# u1# u2# u3# u4#, 4# #)
        _ ->
          (# '\NUL'#, 0# #)
