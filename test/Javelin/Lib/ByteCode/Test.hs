module Javelin.Lib.ByteCode.Test
  ( acceptanceTests
  ) where

import           Data.Word                  (Word16)
import           Javelin.Lib.ByteCode.ClassFile
import           Javelin.Lib.ByteCode.DescSign
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit

import           Data.Map.Strict            (member)

import           Javelin.Lib.ByteCode.Stats     (getStats)
import           Test.Tasty.Providers

acceptanceTests = testGroup "Acceptance tests" [byteCodeTest]
byteCodeTest = testGroup "ByteCode parsing" [statsAndParserTest]
                --,byteCodeParserTest, signatureTest, descriptorTest]

statsAndParserTest =
  testCase "stats" $ do
    stats <- getStats "./acceptance/"
    case stats of
      Left msg -> assertFailure $ "Couldn't parse it! " ++ msg
      Right st -> assertBool "" $ member "aload_0" st
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
