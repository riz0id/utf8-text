{-# LANGUAGE ViewPatterns #-}
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
  -- * Utf8

  -- ** Encode
  ord1,
  ord2,
  ord3,
  ord4,

  -- ** Decode
  chr1,
  chr2,
  chr3,
  chr4,

  -- ** Write
  writeUtf8OffPtr,
  writeByteArrayAsUtf8,

  -- ** Read
  indexByteArrayAsUtf8,
  readByteArrayAsUtf8,

  -- ** Query
  sizeofCharUtf8,
  sizeofLeaderUtf8,
) where

import Control.Monad.Primitive (PrimMonad, PrimState)

import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Unsafe qualified as ByteString
import Data.Char (chr)
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray (..))
import Data.Primitive.ByteArray qualified as ByteArray

import Foreign.Ptr (Ptr, plusPtr)

import GHC.Exts (Word (..))
import GHC.Exts qualified as GHC
import GHC.Storable (writeWord8OffPtr)
import GHC.Word (Word8 (..))

--------------------------------------------------------------------------------

charToWord :: Char -> Word
charToWord = fromIntegral . fromEnum

wordToChar :: Word -> Char
wordToChar = toEnum . fromIntegral

word8ToWord :: Word8 -> Word
word8ToWord (W8# w#) = W# (GHC.word8ToWord# w#)

-- Utf8 ------------------------------------------------------------------------


-- Utf8 - Encode ---------------------------------------------------------------

-- | Cast a character to the equivalent 1-byte UTF-8 encoding.
--
-- @since 1.0.0
ord1 :: Char -> Word8
ord1 = fromIntegral . fromEnum

-- | Cast a character to the equivalent 2-byte UTF-8 encoding.
--
-- @since 1.0.0
ord2 :: Char -> (Word8, Word8)
ord2 (charToWord -> x) =
  let b0 = 0xc0 .|. (0xff .&. shiftR x 6)
      b1 = 0x80 .|. (0x3f .&. shiftR x 0)
   in (fromIntegral b0, fromIntegral b1)

-- | Cast a character to the equivalent 3-byte UTF-8 encoding.
--
-- @since 1.0.0
ord3 :: Char -> (Word8, Word8, Word8)
ord3 (charToWord -> x) =
  let b0 = 0xe0 .|. (0xff .&. shiftR x 12)
      b1 = 0x80 .|. (0x3f .&. shiftR x 6)
      b2 = 0x80 .|. (0x3f .&. shiftR x 0)
   in (fromIntegral b0, fromIntegral b1, fromIntegral b2)

-- | Cast a character to the equivalent 4-byte UTF-8 encoding.
--
-- @since 1.0.0
ord4 :: Char -> (Word8, Word8, Word8, Word8)
ord4 (charToWord -> x) =
  let b0 = 0xf0 .|. (0xff .&. shiftR x 18)
      b1 = 0x80 .|. (0x3f .&. shiftR x 12)
      b2 = 0x80 .|. (0x3f .&. shiftR x 6)
      b3 = 0x80 .|. (0x3f .&. shiftR x 0)
   in (fromIntegral b0, fromIntegral b1, fromIntegral b2, fromIntegral b3)

-- Utf8 - Decode ---------------------------------------------------------------

-- | Decode a single UTF-8 code unit as a character.
--
-- @since 1.0.0
chr1 :: Word8 -> Char
chr1 = toEnum . fromIntegral

-- | Decode two UTF-8 code units as a character.
--
-- @since 1.0.0
chr2 :: Word8 -> Word8 -> Char
chr2 x y =
  let b0 = shiftL (word8ToWord x .&. 0x3f) 6
      b1 = shiftL (word8ToWord y .&. 0x7f) 0
   in wordToChar (b0 + b1)

-- | Decode three UTF-8 code units as a character.
--
-- @since 1.0.0
chr3 :: Word8 -> Word8 -> Word8 -> Char
chr3 x y z =
  let b0 = shiftL (word8ToWord x .&. 0x1f) 12
      b1 = shiftL (word8ToWord y .&. 0x7f) 6
      b2 = shiftL (word8ToWord z .&. 0x7f) 0
   in wordToChar (b0 + b1 + b2)

-- | Decode four UTF-8 code units as a character.
--
-- @since 1.0.0
chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 x y z w =
  let b0 = shiftL (word8ToWord x .&. 0x0f) 18
      b1 = shiftL (word8ToWord y .&. 0x7f) 12
      b2 = shiftL (word8ToWord z .&. 0x7f) 6
      b3 = shiftL (word8ToWord w .&. 0x7f) 0
   in wordToChar (b0 + b1 + b2 + b3)

-- Utf8 - Write ----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
writeUtf8OffPtr :: Ptr Word8 -> Char -> IO (Ptr Word8)
writeUtf8OffPtr ptr c
  | c >= '\x10000' = do
    let (cu1, cu2, cu3, cu4) = ord4 c
    writeWord8OffPtr ptr 0 cu1
    writeWord8OffPtr ptr 1 cu2
    writeWord8OffPtr ptr 2 cu3
    writeWord8OffPtr ptr 3 cu4
    pure (plusPtr ptr 4)
  | c >= '\x800' = do
    let (cu1, cu2, cu3) = ord3 c
    writeWord8OffPtr ptr 0 cu1
    writeWord8OffPtr ptr 1 cu2
    writeWord8OffPtr ptr 2 cu3
    pure (plusPtr ptr 3)
  | c >= '\x80' = do
    let (cu1, cu2) = ord2 c
    writeWord8OffPtr ptr 0 cu1
    writeWord8OffPtr ptr 1 cu2
    pure (plusPtr ptr 2)
  | otherwise = do
    writeWord8OffPtr ptr 0 (ord1 c)
    pure (plusPtr ptr 1)

-- | TODO: docs
--
-- @since 1.0.0
writeByteArrayAsUtf8 ::
  PrimMonad m =>
  -- | TODO: docs
  MutableByteArray (PrimState m) ->
  -- | TODO: docs
  Int ->
  -- | TODO: docs
  Char ->
  -- | TODO: docs
  m Int
writeByteArrayAsUtf8 bxs i c
  | c >= '\x10000' = do
    let (cu1, cu2, cu3, cu4) = ord4 c
    ByteArray.writeByteArray bxs (i + 0) cu1
    ByteArray.writeByteArray bxs (i + 1) cu2
    ByteArray.writeByteArray bxs (i + 2) cu3
    ByteArray.writeByteArray bxs (i + 3) cu4
    pure 4
  | c >= '\x800' = do
    let (cu1, cu2, cu3) = ord3 c
    ByteArray.writeByteArray bxs (i + 0) cu1
    ByteArray.writeByteArray bxs (i + 1) cu2
    ByteArray.writeByteArray bxs (i + 2) cu3
    pure 3
  | c >= '\x80' = do
    let (cu1, cu2) = ord2 c
    ByteArray.writeByteArray bxs (i + 0) cu1
    ByteArray.writeByteArray bxs (i + 1) cu2
    pure 2
  | otherwise = do
    ByteArray.writeByteArray bxs (i + 0) (ord1 c)
    pure 1

-- Utf8 - Read -----------------------------------------------------------------

-- | Read the UTF-8 'Char' at the given index in the 'ByteArray'. Returns the
-- decoded 'Char' along the size of the UTF-8 character in bytes.
--
-- @since 1.0.0
indexByteArrayAsUtf8 :: ByteArray -> Int -> (Char, Int)
indexByteArrayAsUtf8 bxs i
  | len == 1  = (chr (fromIntegral u1), 1)
  | len == 2  =
    let u2 = ByteArray.indexByteArray bxs (i + 1)
     in (chr2 u1 u2, 2)
  | len == 3  =
    let u2 = ByteArray.indexByteArray bxs (i + 1)
        u3 = ByteArray.indexByteArray bxs (i + 2)
     in (chr3 u1 u2 u3, 2)
  | len == 4  =
    let u2 = ByteArray.indexByteArray bxs (i + 1)
        u3 = ByteArray.indexByteArray bxs (i + 2)
        u4 = ByteArray.indexByteArray bxs (i + 3)
     in (chr4 u1 u2 u3 u4, 2)
  | otherwise = ('\NUL', 0)
  where
    u1 :: Word8
    u1 = ByteArray.indexByteArray bxs i

    len :: Int
    len = sizeofLeaderUtf8 u1

-- | TODO: docs
--
-- @since 1.0.0
readByteArrayAsUtf8 ::
  PrimMonad m =>
  -- | TODO: docs
  MutableByteArray (PrimState m) ->
  -- | TODO: docs
  Int ->
  -- | TODO: docs
  m (Char, Int)
readByteArrayAsUtf8 bxs i = do
  u1 <- ByteArray.readByteArray bxs i
  case sizeofLeaderUtf8 u1 of
    1 ->
      pure (chr (fromIntegral u1), 1)
    2 -> do
      u2 <- ByteArray.readByteArray bxs (i + 1)
      pure (chr2 u1 u2, 2)
    3 -> do
      u2 <- ByteArray.readByteArray bxs (i + 1)
      u3 <- ByteArray.readByteArray bxs (i + 2)
      pure (chr3 u1 u2 u3, 3)
    4 -> do
      u2 <- ByteArray.readByteArray bxs (i + 1)
      u3 <- ByteArray.readByteArray bxs (i + 2)
      u4 <- ByteArray.readByteArray bxs (i + 3)
      pure (chr4 u1 u2 u3 u4, 4)
    _ ->
      pure ('\NUL', 0)

-- Utf8 - Query ----------------------------------------------------------------

utf8LeaderLengthTable :: ByteString
utf8LeaderLengthTable = ByteString.pack [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0]

-- | Obtain the number of UTF-8 code units that are required to encode the
-- given 'Char' in UTF-8.
--
-- @since 1.0.0
sizeofCharUtf8 :: Char -> Int
sizeofCharUtf8 x
  | x >= '\x10000' = 4
  | x >= '\x800'   = 3
  | x >= '\x80'    = 2
  | otherwise      = 1

-- | Given a valid UTF-8 leader byte, obtain the number of UTF-8 code units
-- used to encode the UTF-8 character.
--
-- >>> sizeofLeaderUtf8 0x61 -- The UTF-8 representation of 'a'
-- 1
--
-- @since 1.0.0
sizeofLeaderUtf8 :: Word8 -> Int
sizeofLeaderUtf8 leader = fromIntegral (ByteString.unsafeIndex utf8LeaderLengthTable ix)
  where
    ix :: Int
    ix = fromIntegral (shiftR leader 3)
