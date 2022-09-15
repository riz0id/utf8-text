{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Text.Utf8
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
module Text.Utf8
  ( -- * Index
    byte1,
    byte2,
    byte3,
    byte4,

    -- * Query
    sizeUtf8,

    -- * Predicates
    isUtf8,
    isContinuationByte,
    isSurrogate,

    -- * I/O
    readUtf8,
    hGetBuf,
    hGetBufSome,
    decodeBlockUtf8,
  )
where

import Data.Bits ((.&.))
import Data.Bool.Prim qualified as Bool
import Data.Primitive (Ptr)
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray (MutableByteArray))
import Data.Primitive.ByteArray qualified as ByteArray

import GHC.ForeignPtr (ForeignPtr, plusForeignPtr, withForeignPtr)
import GHC.ForeignPtr qualified as ForeignPtr
import GHC.Exts (Int (I#), RealWorld, Int#, Word32#)
import GHC.Word (Word16 (W16#), Word32 (W32#), Word8 (W8#))

import System.IO (Handle)
import System.IO qualified as IO

--------------------------------------------------------------------------------

import Text.Utf8.Prim qualified as Prim
import qualified GHC.Exts as GHC
import Control.Monad (when)
import Foreign.Storable
import Foreign.Ptr (IntPtr (IntPtr))

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
byte1 :: Word32 -> Word8
byte1 (W32# x#) = W8# (Prim.byte1# x#)
{-# INLINE byte1 #-}

-- | TODO
--
-- @since 1.0.0
byte2 :: Word32 -> Word8
byte2 (W32# x#) = W8# (Prim.byte1# x#)
{-# INLINE byte2 #-}

-- | TODO
--
-- @since 1.0.0
byte3 :: Word32 -> Word8
byte3 (W32# x#) = W8# (Prim.byte1# x#)
{-# INLINE byte3 #-}

-- | TODO
--
-- @since 1.0.0
byte4 :: Word32 -> Word8
byte4 (W32# x#) = W8# (Prim.byte1# x#)
{-# INLINE byte4 #-}

-- Query -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
sizeUtf8 :: Word8 -> Int
sizeUtf8 (W8# x#) = I# (Prim.sizeUtf8# x#)
{-# INLINE sizeUtf8 #-}

-- Predicates ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
isUtf8 :: Word32 -> Bool
isUtf8 x =
  case sizeUtf8 (byte1 x) of
    1 -> True
    2 -> 0xC0800000 == x .&. 0xC0800000
    3 -> 0xE0808000 == x .&. 0xE0808000
    4 -> 0xF0808080 == x .&. 0xF0808080
    _ -> False
{-# INLINE isUtf8 #-}

-- | TODO
--
-- @since 1.0.0
isContinuationByte :: Word8 -> Bool
isContinuationByte (W8# x#) = Bool.toBool (Prim.isContinuationByte# x#)
{-# INLINE isContinuationByte #-}

-- | TODO
--
-- @since 1.0.0
isSurrogate :: Word16 -> Bool
isSurrogate (W16# x#) = Bool.toBool (Prim.isSurrogate# x#)
{-# INLINE isSurrogate #-}

-- I/O -------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
readUtf8 :: FilePath -> IO ByteArray
readUtf8 filepath =
  IO.withFile filepath IO.ReadMode \hdl -> do
    IO.hSetBuffering hdl (IO.BlockBuffering $ Nothing)

    len <- fmap fromInteger (IO.hFileSize hdl)
    dst <- ByteArray.newAlignedPinnedByteArray (4 * len) 4
    ByteArray.fillByteArray dst 0 (4 * len) 0

    (cnt, len') <- hGetBuf hdl (newForeignByteArray dst) len
    bxs <- ByteArray.resizeMutableByteArray dst (4 * cnt)
    
    print $ "file length: " ++ show len
    print $ "bytes read: " ++ show cnt
    print $ "final byte array length: " ++ show len'

    ByteArray.unsafeFreezeByteArray bxs
  where 
    newForeignByteArray :: MutableByteArray RealWorld -> ForeignPtr Word32
    newForeignByteArray (MutableByteArray bxs#) =
      let addr = GHC.mutableByteArrayContents# bxs#
          ctnt = ForeignPtr.PlainPtr bxs#
       in ForeignPtr.ForeignPtr addr ctnt

-- | TODO
--
-- @since 1.0.0
hGetBuf :: Handle -> ForeignPtr Word32 -> Int -> IO (Int, Int)
hGetBuf hdl dst dstLen = do
  bufLen <- hGetInitialBufferSize
  bufPtr <- ForeignPtr.mallocForeignPtrBytes (4 + bufLen) -- 4 byte buffer

  print $ "initial swap buffer length: " ++ show bufLen

  let loop :: Int -> Int -> IO (Int, Int)
      loop cnt len 
        | cnt < dstLen = do
          let dstPtr = ForeignPtr.plusForeignPtr dst (4 * cnt)
          (nCnt, nLen) <- hGetBufSome hdl bufPtr dstPtr (min (bufLen - cnt) bufLen)
          loop (nCnt + cnt) (nLen + len)
        | otherwise = pure (len, cnt)
  
  result <- loop 0 0
  ForeignPtr.finalizeForeignPtr bufPtr
  pure result
  where
    hGetInitialBufferSize :: IO Int
    hGetInitialBufferSize = do
      buffering <- IO.hGetBuffering hdl
      case buffering of
        IO.NoBuffering -> fmap fromInteger (IO.hFileSize hdl)
        IO.LineBuffering -> pure (min 1024 dstLen)
        IO.BlockBuffering x -> pure (fromMaybe (min 1024 dstLen) x)

-- | TODO
--
-- @since 1.0.0
hGetBufSome :: Handle -> ForeignPtr Word8 -> ForeignPtr Word32 -> Int -> IO (Int, Int)
hGetBufSome hdl buf dst n =
  withForeignPtr buf \ptr ->
    (\cnt len -> (cnt, len))
      <$> IO.hGetBufSome hdl ptr n
      <*> decodeBlockUtf8 buf dst n
{-# INLINE hGetBufSome #-}

-- | TODO
--
-- @since 1.0.0
decodeBlockUtf8 :: ForeignPtr Word8 -> ForeignPtr Word32 -> Int -> IO Int
decodeBlockUtf8 src dst len = do 
  withForeignPtr src \srcPtr ->
    withForeignPtr dst \dstPtr ->
      unsafeDecodeBlockUtf8 srcPtr dstPtr len
{-# INLINE decodeBlockUtf8 #-}

-- | TODO
--
-- @since 1.0.0
foreign import ccall unsafe "utf8_decode_block"
  unsafeDecodeBlockUtf8 :: Ptr Word8 -> Ptr Word32 -> Int -> IO Int