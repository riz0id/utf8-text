{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Utf8.TH
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
module Data.Utf8.TH (
  defineStaticIntArrayQ,
) where

import Data.Primitive.Ptr (writeOffPtr)

import Foreign.ForeignPtr
  (castForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Storable (sizeOf)

import Language.Haskell.TH (Exp (..), Lit (..), Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Bytes (..))

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
defineStaticIntArrayQ :: [Int] -> Q Exp
defineStaticIntArrayQ = fmap (LitE . BytesPrimL) . TH.runIO . intsToBytesIO

-- | TODO: docs
--
-- @since 1.0.0
intsToBytesIO :: [Int] -> IO Bytes
intsToBytesIO xs = do
  let size = length xs * sizeOf (0 :: Int)
  buf <- mallocForeignPtrBytes size
  withForeignPtr buf (iforeach_ xs . writeOffPtr)
  pure (Bytes (castForeignPtr buf) 0 (fromIntegral size))

-- | TODO: docs
--
-- @since 1.0.0
iforeach_ :: [a] -> (Int -> a -> IO ()) -> IO ()
iforeach_ xs0 f = run 0 xs0 where
  run _ []       = pure ()
  run i (x : xs) = f i x *> run (1 + i) xs
