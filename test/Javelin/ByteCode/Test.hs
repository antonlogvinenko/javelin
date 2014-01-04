module Javelin.ByteCode.Test (byteCodeTest)
where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Javelin.ByteCode
import Data.Word (Word16)

byteCodeTest = testGroup "ByteCode parser test" [byteCodeParserTest,
                                                requireTest,
                                                update2bytesTest]

requireTest = let t = (Left "error", [0x00])
              in testGroup "require" [
                      testCase "required" $
                               require 2 [0x00, 0x00] t @=? t,
                      testCase "nope" $
                               require 3 [0x00, 0x00] t @=? (Left "Unexpected EOF", [0x00, 0x00])
               ]

update2bytesTest = testGroup "bytes update" [
                    testCase "bla" $
                                 (upd2bytes [1, 2] (\v -> emptyClassDef {minVer = v})
                                                @=? (Right $ emptyClassDef {minVer = 258}, []))
                   ]


byteCodeParserTest = testGroup "ByteCode parser" [
                      testCase "parse" $
                                   (parse bytecode) @=? (Left "Unexpected EOF")
                     ]



bytecode = [0xCA, 0xFE, 0xBA, 0xBE,
            0x00, 0x00,
            0x00, 0x00
           ]
            
