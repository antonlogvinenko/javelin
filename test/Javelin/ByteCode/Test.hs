module Javelin.ByteCode.Test (byteCodeTest)
where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Javelin.ByteCode.ClassFile
import Data.Word (Word16)

byteCodeTest = testGroup "ByteCode parser test" [byteCodeParserTest]

byteCodeParserTest = testGroup "ByteCode parser" [
                      -- testCase "parse" $
                      --              (parse bytecode) @=? (Left "Undefined constant")
                     ]

bytecode = [0xCA, 0xFE, 0xBA, 0xBE,
            0x00, 0x00,
            0x00, 0x00,
            0x00, 0x00,
            0x00, 0x00
           ]
            
