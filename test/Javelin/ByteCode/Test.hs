module Javelin.ByteCode.Test (byteCodeTest)
where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Javelin.ByteCode.ClassFile
import Data.Word (Word16)

byteCodeTest = testGroup "ByteCode parser test" [byteCodeParserTest,
                                                requireTest
                                                 ]

requireTest = let t = "bla"
              in testGroup "require" [
                      testCase "required" $
                               require 2 [0x00, 0x00] t @=? Right t,
                      testCase "nope" $
                               require 3 [0x00, 0x00] t @=? Left "Unexpected EOF"
               ]


byteCodeParserTest = testGroup "ByteCode parser" [
                      testCase "parse" $
                                   (parse bytecode) @=? (Left "Unexpected EOF")
                     ]



bytecode = [0xCA, 0xFE, 0xBA, 0xBE,
            0x00, 0x00,
            0x00, 0x00
           ]
            
