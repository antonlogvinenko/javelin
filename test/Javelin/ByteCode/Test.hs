module Javelin.ByteCode.Test (byteCodeTest)
where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Javelin.ByteCode.ClassFile
import Javelin.ByteCode.DescSign
import Data.Word (Word16)
import Javelin.Main

byteCodeTest = testGroup "ByteCode parser test"
               [byteCodeParserTest,
                signatureTest, descriptorTest]

byteCodeParserTest = 
  testCase "functional" $
  do
    (io, result) <- testClasses "/Users/anton/dev/haskell/javelin/acceptance/"
--    sequence_ $ map (putStrLn . show) io
    result @=? True

-- These tests need refactoring

signatureTest = testGroup "signatures" [
  testCase "class signature" $
  validate (parseClassSignature "<T:Ljava/lang/Object;>Ljava/lang/Object;") @=? True,

  testGroup "method signatures" [
    testCase "1" $
    validate (parseMethodSignature "<T:Ljava/lang/Object;>(TT;)Lorg/springframework/http/HttpEntity<TT;>;" ) @=? True,

    testCase "2" $
    validate (parseMethodSignature "(Lorg/apache/lucene/search/Query;Lcom/farpost/search/Sort;ILjava/util/Map<Ljava/lang/String;Lorg/apache/lucene/search/Query;>;)Lcom/farpost/search/IndexResult;") @=? True,

    testCase "3" $
    validate (parseMethodSignature "(Lorg/apache/lucene/search/Query;Lcom/farpost/search/Sort;ILjava/util/Map;)Lcom/farpost/search/IndexResult;") @=? True
              ]
  ]

descriptorTest = testGroup "descriptors" [
  testCase "method descriptor" $
  validate (parseMethodDescriptor "(Lorg/apache/lucene/search/Query;Lcom/farpost/search/Sort;ILjava/util/Map;)Lcom/farpost/search/IndexResult;") @=? True,

  testCase "class descriptor" $
  validate (parseFieldDescriptor "Lcom/farpost/search/index/QueryMapper;") @=? True
  ]
