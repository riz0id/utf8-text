{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
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
  defineUtf8LeaderLengthTable
) where

import Data.Primitive.ByteArray (MutableByteArray (..))
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Utf8.Compat (keepAliveUnlifted)

import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (..))

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH (Exp, Q)
import qualified GHC.Exts as GHC
import Data.Word (Word8)
import Control.Monad (zipWithM_)

--------------------------------------------------------------------------------

defineUtf8LeaderLengthTable :: Q Exp
defineUtf8LeaderLengthTable = TH.runIO do
  bxs@(MutableByteArray bxs#) <- ByteArray.newPinnedByteArray 32

  zipWithM_ (ByteArray.writeByteArray @Word8 bxs) [0 .. 31] contents

  keepAliveUnlifted bxs#
    let !ptr# = GHC.mutableByteArrayContents# bxs#
        fp    = ForeignPtr ptr# (PlainPtr bxs#)
        bytes = TH.mkBytes fp 0 32
     in pure (TH.LitE (TH.BytesPrimL bytes))
  where
    contents :: [Word8]
    contents = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0]