module Javelin.ByteCode.Test (byteCodeTest)
where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Javelin.ByteCode.ClassFile
import Data.Word (Word16)
import Javelin.Main

byteCodeTest = testGroup "ByteCode parser test" [byteCodeParserTest]

byteCodeParserTest = testGroup "ByteCode parser" [
  testCase "functional" $
  do
    (io, result) <- testClasses "/Users/anton/dev/haskell/javelin/acceptance/"
--    sequence_ $ map (putStrLn . show) io
    result @=? True
  ]
