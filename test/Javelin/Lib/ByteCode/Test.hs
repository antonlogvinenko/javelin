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
import Data.Text (unpack, strip, pack)

rtPath =
  "sample-classpath/rt.jar:test-programs-output/"

compileJava :: String -> IO String
compileJava className = readProcess "javac" ["-d", "test-programs-output/", "test-programs/javelin/" ++ className ++ ".java"] ""

-- temporary pack/unpack: don't want to introduce missingH for `strip` but also don't want to switch everything to text right now
executeMainClass :: String -> IO String
executeMainClass className =  unpack . strip . pack <$> readProcess "cabal" ["run", "--verbose=0", "javelin", "jvm", "javelin." ++ className, rtPath, "1"] ""

-- cabal run --verbose=0 javelin jvm javelin.SumOfIntegers sample-classpath/rt.jar:test-programs-output 1
-- javac -d /Users/anton/dev/haskell/javelin/test-programs-output test-programs/javelin/demo/App.java
-- java -cp test-programs-output javelin.demo.App

javelinTests =
  testGroup
    "Acceptance tests"
    [ testGroup "Unit tests" [testGroup "ByteCode parsing" [statsAndParserTest]]
    , testGroup
        "Sample testing"
        [
          executionTest "sum of integers" "SumOfIntegers" "3" --iconst1 istore1 iconst2 istore2 iload1 iload2 iadd istore3 iload3
          , executionTest "sum of longs" "SumOfLongs" "1" --lconst0 lstore1 lconst1 lstore3 lload1 lload3 ladd lstore5 lload5 -- lload3 does not work
          , executionTest "sum of floats" "SumOfFloats" "1.0" --iconst1 istore1 iconst2 istore2 iload1 iload2 iadd istore3 iload3
          ]
    ]

    -- enrich with other store/loads for ints. longs, floats
    -- same for other arithmetic ops

executionTest :: String -> String -> String -> TestTree
executionTest testName className expectedResult =
  testCaseSteps testName $ \step -> do
    compileJava className
    output <- executeMainClass className
    assertEqual testName expectedResult output

statsAndParserTest =
  testCase "stats" $ do
    stats <- getStats "acceptance/"
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
