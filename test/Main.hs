import Javelin.Lib.ByteCode.Test
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Javelin tests" [javelinTests]
