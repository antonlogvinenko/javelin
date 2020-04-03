module Javelin.Lib.ByteCode.Test
  ( javelinTests
  ) where

import Javelin.Lib.ByteCode.ClassFile ()
import Javelin.Lib.ByteCode.DescSign ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Data.Map.Strict (member)

import Javelin.Lib.ByteCode.Stats (getStats)
import Test.Tasty.Providers ()

import System.Process (readProcess)

rtPath =
  "/Library/Java/JavaVirtualMachines/openjdk8/Contents/Home/jre/lib/rt.jar:main"

stackPath = "/usr/local/bin/stack"

executeMainClass :: String -> IO String
executeMainClass className =
  readProcess
    stackPath
    ["exec", "--", "javelin", "jvm", className, rtPath, "1"]
    ""

-- javac -d /Users/anton/dev/haskell/javelin/test-programs-output test-programs/javelin/demo/App.java
-- java -cp test-programs-output javelin.demo.App

javelinTests =
  testGroup
    "Acceptance tests"
    [ testGroup "Unit tests" [testGroup "ByteCode parsing" [statsAndParserTest]]
    , testGroup
        "Sample testing"
        [executionTest "sum of integers" "test.App" "3"]
    ]

executionTest :: String -> String -> String -> TestTree
executionTest testName className expectedResult =
  testCase testName $ do
    output <- executeMainClass className
    assertEqual testName expectedResult output

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
