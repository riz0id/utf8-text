{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Utf8
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
module Data.Utf8 (
  -- * ByteArray
  indexUtf8Array,
  readUtf8Array,
  writeUtf8Array,

  -- * Encoding
  ord1,
  ord2,
  ord3,
  ord4,

  -- * Decoding
  chr1,
  chr2,
  chr3,
  chr4,

  -- * Query
  lengthUtf8Char,
  lengthUtf8Word8,
  lengthUtf8Word16,
  lengthUtf8Word32,
) where

import Control.Monad.Primitive (PrimMonad, PrimState, primitive)

import Data.Primitive.ByteArray (MutableByteArray (..), ByteArray (..))
import Data.Utf8.Prim qualified as Prim

import GHC.Exts (Char (C#), Int (I#))
import GHC.Word (Word16 (W16#), Word32 (W32#), Word8 (W8#))

-- ByteArray -------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
indexUtf8Array :: ByteArray -> Int -> (Char, Int)
indexUtf8Array (ByteArray src#) (I# i#) = 
  case Prim.indexUtf8Array# src# i# of 
    (# c#, n# #) -> (C# c#, I# n#)

-- | TODO: docs
--
-- @since 1.0.0
readUtf8Array :: 
  PrimMonad m => 
  MutableByteArray (PrimState m) -> 
  Int -> 
  m (Char, Int)
readUtf8Array (MutableByteArray src#) (I# i#) = 
  primitive \st0# -> case Prim.readUtf8Array# src# i# st0# of 
    (# st1#, c#, n# #) -> (# st1#, (C# c#, I# n#) #)

-- | TODO: docs
--
-- @since 1.0.0
writeUtf8Array :: 
  PrimMonad m => 
  -- | TODO: docs
  MutableByteArray (PrimState m) -> 
  -- | TODO: docs
  Int -> 
  -- | TODO: docs
  Char -> 
  -- | TODO: docs
  m Int
writeUtf8Array (MutableByteArray dst#) (I# i#) (C# c#) = 
  primitive \st0# -> case Prim.writeUtf8Array# dst# i# c# st0# of 
    (# st1#, n# #) -> (# st1#, I# n# #)

-- Encoding --------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
ord1 :: Char -> Word8
ord1 (C# c#) = W8# (Prim.ord1# c#)
{-# INLINE ord1 #-}

-- | TODO: docs
--
-- @since 1.0.0
ord2 :: Char -> (Word8, Word8)
ord2 (C# c#) = case Prim.ord2# c# of 
  (# x#, y# #) -> (W8# x#, W8# y#)
{-# INLINE ord2 #-}

-- | TODO: docs
--
-- @since 1.0.0
ord3 :: Char -> (Word8, Word8, Word8)
ord3 (C# c#) = case Prim.ord3# c# of 
  (# x#, y#, z# #) -> (W8# x#, W8# y#, W8# z#)
{-# INLINE ord3 #-}

-- | TODO: docs
--
-- @since 1.0.0
ord4 :: Char -> (Word8, Word8, Word8, Word8)
ord4 (C# c#) = case Prim.ord4# c# of 
  (# x#, y#, z#, w# #) -> (W8# x#, W8# y#, W8# z#, W8# w#)
{-# INLINE ord4 #-}

-- Decoding --------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
chr1 :: Word8 -> Char 
chr1 (W8# x#) = C# (Prim.chr1# x#)
{-# INLINE chr1 #-}

-- | TODO: docs
--
-- @since 1.0.0
chr2 :: Word8 -> Word8 -> Char 
chr2 (W8# x#) (W8# y#) = C# (Prim.chr2# x# y#)
{-# INLINE chr2 #-}

-- | TODO: docs
--
-- @since 1.0.0
chr3 :: Word8 -> Word8 -> Word8 -> Char 
chr3 (W8# x#) (W8# y#) (W8# z#) = C# (Prim.chr3# x# y# z#)
{-# INLINE chr3 #-}

-- | TODO: docs
--
-- @since 1.0.0
chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char 
chr4 (W8# x#) (W8# y#) (W8# z#) (W8# w#) = C# (Prim.chr4# x# y# z# w#)
{-# INLINE chr4 #-}

-- Query -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
lengthUtf8Char :: Char -> Int
lengthUtf8Char (C# c#) = I# (Prim.lengthUtf8Char# c#)
{-# INLINE lengthUtf8Char #-}

-- | TODO
--
-- @since 1.0.0
lengthUtf8Word8 :: Word8 -> Int
lengthUtf8Word8 (W8# u8#) = I# (Prim.lengthUtf8Word8# u8#)
{-# INLINE lengthUtf8Word8 #-}

-- | TODO
--
-- @since 1.0.0
lengthUtf8Word16 :: Word16 -> Int
lengthUtf8Word16 (W16# u16#) = I# (Prim.lengthUtf8Word16# u16#)
{-# INLINE lengthUtf8Word16 #-}

-- | TODO
--
-- @since 1.0.0
lengthUtf8Word32 :: Word32 -> Int
lengthUtf8Word32 (W32# u32#) = I# (Prim.lengthUtf8Word32# u32#)
{-# INLINE lengthUtf8Word32 #-}
