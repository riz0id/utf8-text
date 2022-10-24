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
-- TODO
--
-- @since 1.0.0
module Data.Utf8 (
  -- toUtf8,
  -- lengthByLeader,

  -- * Query
  -- lengthUtf8Char,
  lengthUtf8Word8,
  lengthUtf8Word16,
  lengthUtf8Word32,
) where

import GHC.Exts (Int (I#))
import GHC.Word (Word8 (W8#), Word16 (W16#), Word32 (W32#))

--------------------------------------------------------------------------------

import Data.Utf8.Prim qualified as Prim

--------------------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0 
-- toUtf8 :: Char -> Word32
-- toUtf8 (C# c#) = W32# (Prim.toUtf8# c#)
-- {-# INLINE toUtf8 #-}

-- Query -----------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0 
-- lengthUtf8 :: Char# -> Int# 
-- lengthUtf8 (C# c#) = I# (Prim.lengthUtf8# c#)
-- {-# INLINE lengthUtf8 #-}

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
