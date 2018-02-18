module Javelin.ByteCode.Test
  ( byteCodeTest
  ) where

import           Data.Word                  (Word16)
import           Javelin.ByteCode.ClassFile
import           Javelin.Runtime.DescSign
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit

import           Data.Map.Strict            (member)

--import Main
import           Javelin.ByteCode.Stats     (getStats)
import           Test.Tasty.Providers

byteCodeTest = testGroup "ByteCode parser test" [statsAndParserTest]
                --,byteCodeParserTest, signatureTest, descriptorTest]

statsAndParserTest =
  testCase "stats" $ do
    stats <- getStats "./acceptance/"
    case stats of
      Left msg -> assertFailure $ "Couldn't parse it! " ++ msg
      Right st -> assertBool "" $ member "aload_0" st
-- byteCodeParserTest =
--   testCase "functional" $
--   do
--     (io, result) <- testClasses "./acceptance/"
--     result @=? True
-- -- These tests need refactoring
-- signatureTest = testGroup "signatures" [
--   testCase "class signature" $
--   validate (parseClassSignature "<T:Ljava/lang/Object;>Ljava/lang/Object;") @=? True,
--   testGroup "method signatures" [
--     testCase "1" $
--     validate (parseMethodSignature "<T:Ljava/lang/Object;>(TT;)Lorg/springframework/http/HttpEntity<TT;>;" ) @=? True,
--     testCase "2" $
--     validate (parseMethodSignature "(Lorg/apache/lucene/search/Query;Lcom/farpost/search/Sort;ILjava/util/Map<Ljava/lang/String;Lorg/apache/lucene/search/Query;>;)Lcom/farpost/search/IndexResult;") @=? True,
--     testCase "3" $
--     validate (parseMethodSignature "(Lorg/apache/lucene/search/Query;Lcom/farpost/search/Sort;ILjava/util/Map;)Lcom/farpost/search/IndexResult;") @=? True
--               ]
--   ]
-- descriptorTest = testGroup "descriptors" [
--   testCase "method descriptor" $
--   validate (parseMethodDescriptor "(Lorg/apache/lucene/search/Query;Lcom/farpost/search/Sort;ILjava/util/Map;)Lcom/farpost/search/IndexResult;") @=? True,
--   testCase "class descriptor" $
--   validate (parseFieldDescriptor "Lcom/farpost/search/index/QueryMapper;") @=? True
--   ]
