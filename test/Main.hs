import Test.Tasty (defaultMain, testGroup)
import Javelin.ByteCode.Test

main = defaultMain $ testGroup "Some tests" [byteCodeTest]

