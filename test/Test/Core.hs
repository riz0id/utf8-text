module Test.Core (
  Property,
  TestTree,
  property,
  testCase,
  testCases,
  testGroup,
  testProp,
  testPropWithCases,
) where

import Hedgehog (Property, PropertyT, TestLimit, property, withTests)

import Test.Compat (testProp)
import Test.Tasty (TestTree, TestName, testGroup)

--------------------------------------------------------------------------------

testCase :: TestName -> PropertyT IO () -> TestTree 
testCase name = testProp name . property

testCases :: TestName -> TestLimit -> PropertyT IO () -> TestTree 
testCases name n = testPropWithCases name n . property

testPropWithCases :: String -> TestLimit -> Property -> TestTree
testPropWithCases name n = testProp name . withTests n