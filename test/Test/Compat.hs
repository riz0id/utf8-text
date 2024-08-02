{-# LANGUAGE CPP #-}

module Test.Compat
  ( TestTree,
    testGroup,
    testProp,
  )
where

import Data.String (fromString)

import Hedgehog (Property)

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog qualified as Tasty

--------------------------------------------------------------------------------

#if MIN_VERSION_tasty_hedgehog(1,2,0)

testProp :: TestName -> Property -> TestTree
testProp name = Tasty.testPropertyNamed name (fromString name)

#else

testProp :: TestName -> Property -> TestTree
testProp = Tasty.testProperty

#endif
