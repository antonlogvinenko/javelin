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

upd EmptyClassDef _ = EmptyClassDef
upd cd v = cd {minVer = v, majVer = v, constPoolSize = v, constPool = []}
              
update2bytesTest = let t = EmptyClassDef :: ClassDef
                   in testGroup "bytes update" [
                           testCase "bla" $
                                    (upd2bytes [0x01, 0x02] (upd t)
                                                  @=? (Right t, []))
                    ]


byteCodeParserTest = testGroup "ByteCode parser" [
                      testCase "parse" $
                                   (parse bytecode) @=? (Left "Unexpected EOF")
                     ]



bytecode = [0xCA, 0xFE, 0xBA, 0xBE,
            0x00, 0x00,
            0x00, 0x00
           ]
            
