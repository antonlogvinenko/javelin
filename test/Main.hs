import Javelin.Lib.ByteCode.Test
import Test.Tasty (defaultMain, testGroup)

main = defaultMain $ testGroup "Test root" [acceptanceTests]
