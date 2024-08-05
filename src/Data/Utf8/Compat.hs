{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Utf8.Compat
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
module Data.Utf8.Compat (
    keepAliveUnlifted
  , keepAliveUnliftedLifted#
) where

import Control.Monad.Primitive (PrimBase (..), PrimMonad (..))

import Data.Kind (Type)

import GHC.Exts (RealWorld, State#, UnliftedType)
import qualified GHC.Exts as GHC

--------------------------------------------------------------------------------

keepAliveUnliftedLifted# ::
  forall (s :: Type) (a :: UnliftedType) (b :: Type).
  a ->
  State# s ->
  (State# s -> (# State# s, b #)) ->
  (# State# s, b #)
keepAliveUnliftedLifted# x s0 f =
  (GHC.unsafeCoerce# :: (# State# RealWorld, b #) -> (# State# s, b #))
    ( GHC.keepAlive# x
      ((GHC.unsafeCoerce# :: State# s -> State# RealWorld) s0)
      ((GHC.unsafeCoerce# ::
         (State# s -> (# State# s, b #)) ->
         (State# RealWorld -> (# State# RealWorld, b #))
       ) f)
    )
{-# INLINE keepAliveUnliftedLifted# #-}

keepAliveUnlifted :: forall (m :: Type -> Type) (a :: UnliftedType) (r :: Type). PrimBase m => a -> m r -> m r
keepAliveUnlifted x k = primitive \s0 -> keepAliveUnliftedLifted# x s0 (internal k)
{-# INLINE keepAliveUnlifted #-}
