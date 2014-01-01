module Javelin.ByteCode
       (byteCodeSuite)
where
  import Test.Tasty (testGroup, TestTree)
  import Test.Tasty.HUnit
  import Javelin.ByteCode

byteCodeSuite :: TestTree
byteCodeSuite = testGroup "cake" [testCase "test" testTest]

testTest :: Assertion
testTest = "abc" @=? "abcd"

       
