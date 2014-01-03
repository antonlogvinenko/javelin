module Javelin.ByteCode.Test (byteCodeTest)
where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit

byteCodeTest = testGroup "bla" [testCase "cake test" someTest]

someTest :: Assertion
someTest = "abc" @=? "abc"
