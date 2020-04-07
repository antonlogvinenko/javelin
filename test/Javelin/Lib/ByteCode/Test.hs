module Javelin.Lib.ByteCode.Test
  ( javelinTests
  ) where

import Javelin.Lib.ByteCode.ClassFile ()
import Javelin.Lib.ByteCode.DescSign ()
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit

import Data.Map.Strict (member)

import Javelin.Lib.ByteCode.Stats (getStats)
import Test.Tasty.Providers ()

import Data.Text (pack, strip, unpack)
import System.Process (readProcess)
import System.Directory (listDirectory)
import Data.List ( isSuffixOf )

rtPath = "sample-classpath/rt.jar:test-programs-output/"

isJavaFile :: String -> Bool
isJavaFile file = ".java" `isSuffixOf` file

compileAll :: IO String
compileAll = do
  files <- listDirectory "test-programs/javelin"
  let javaFiles = filter isJavaFile files :: [FilePath]
  let compilePath = map ("test-programs/javelin/" ++) javaFiles :: [String]
  readProcess
    "javac"
    ([
      "-d"
    , "test-programs-output/"
    ] ++ compilePath)
    ""

-- temporary pack/unpack: don't want to introduce missingH for `strip` but also don't want to switch everything to text right now
executeMainClass :: String -> IO String
executeMainClass className =
  unpack . strip . pack <$>
  readProcess
    "cabal"
    [ "run"
    , "--verbose=0"
    , "javelin"
    , "jvm"
    , "javelin." ++ className
    , rtPath
    , "1"
    ]
    ""

-- cabal run --verbose=0 javelin jvm javelin.SumOfIntegers sample-classpath/rt.jar:test-programs-output 1
-- javac -d /Users/anton/dev/haskell/javelin/test-programs-output test-programs/javelin/demo/App.java
-- java -cp test-programs-output javelin.demo.App
javelinTests =
  testGroup
    "Acceptance tests"
    [
       testGroup "Unit tests" [testGroup "ByteCode parsing" [statsAndParserTest]],
       withResource compileAll (\_ -> print "release") $ \_ -> testGroup
        "Sample testing"
        [ jvmTest "SumOfIntegers" "4" --covers iconst0 iconst1 istore1 iconst2 istore2 iload1 iload2 iadd istore3 iload3 return istore iload
        , jvmTest "SumOfLongs" "1" --covers lconst0 lstore1 lconst1 lstore3 lload1 lload3 ladd lstore lload return
        , jvmTest "SumOfFloats" "3.0" --covers fconst0 fstore1 fconst1 fstore2 fconst2 fstore3 fload1 fload2 fadd fload3 fadd fstore fload
        , jvmTest "SumOfDoubles" "2.0" --covers dconst0 dstore1 dconst1 dstore3 dload1 dload3 dadd dload dstore
        , jvmTest "MulOfIntegers" "60" --covers iconst3 iconst4 iconst5 imul
        , jvmTest "MulOfLongs" "1" --covers lmul
        , jvmTest "MulOfFloats" "4.0" --covers fmul
        , jvmTest "MulOfDoubles" "1.0" --covers dmul
        , jvmTest "SubOfIntegers" "1" --covers isub
        , jvmTest "SubOfLongs" "1" --covers lsub
        , jvmTest "SubOfFloats" "1.0" --covers fsub
        , jvmTest "SubOfDoubles" "1.0" --covers dsub
        , jvmTest "NegInteger" "-2" --covers ineg
        , jvmTest "NegLong" "-1" --covers lneg
        , jvmTest "NegFloats" "-2.0" --covers fneg
        , jvmTest "NegDouble" "-1.0" --covers dneg
        , jvmTest "OrOfIntegers" "3" --covers ior
        , jvmTest "AndOfIntegers" "0" --covers iand
        , jvmTest "XorOfIntegers" "7" --covers ixor
        , jvmTest "OrOfLongs" "3" --covers lor
        , jvmTest "AndOfLongs" "0" --covers land
        , jvmTest "XorOfLongs" "11" --covers lxor, ldc2w
        ]
    ]

-- 0) use turtle to work directory stuff
-- 1) tests for: ishl ishr iushr lshl lshr lushr
-- 2) [div rem] X [int double float long]
-- 3) [cmpg] X [int double float long]
-- 4) iinc
-- 5) jint stores both info
-- 6) jint is passed to push instead of type annotation
-- 7) code fetchest first or second from jint when it needs to write/read word64/32

jvmTest :: String -> String -> TestTree
jvmTest className expectedResult =
  testCaseSteps className $ \step -> do
    output <- executeMainClass className
    assertEqual className expectedResult output

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
