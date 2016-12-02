module Javelin.ByteCode.Stats (stats)
where

import Javelin.ByteCode.ClassFile (parse)
import qualified Data.Map.Lazy as Map
import Control.Monad
import System.Directory
import Data.Word (Word8)
import Data.Binary.Get
import Javelin.ByteCode.Data
import Data.Foldable

import qualified Data.ByteString.Lazy as LBS (ByteString)
import qualified Data.ByteString as BS (unpack, readFile)

-- all instructions
--- handle many files: read each file's full content only on parse
-- pretty printing
-- correct fold?
-- error messages

stats :: FilePath -> IO String
stats path = do
  files <- getDirectoryContents path
  let filePaths = map (path ++) .filter (`notElem` [".", ".."]) $ files
  parseResults <- (mapM parseFileContents filePaths) :: IO [Either String ByteCode]
  let groupParseResult = sequence parseResults :: Either String [ByteCode]
  return $ case groupParseResult of
    Left msg -> msg
    Right bcs -> show $ foldl' (flip addFreq) mempty bcs

addFreq :: ByteCode -> Map.Map OpCode Integer -> Map.Map OpCode Integer
addFreq bc freq = let opCodes = do
                        method <- methods $ body bc
                        attr <- methodAttrs method
                        guard (isCodeAttr attr)
                        instruction <- code attr
                        return $ oc instruction
                  in composeFreqs freq opCodes

composeFreqs :: Map.Map OpCode Integer -> [OpCode] -> Map.Map OpCode Integer
composeFreqs x [] = x
composeFreqs x (c:cs) = composeFreqs (Map.alter statsAlter c x) cs
  where statsAlter = Just . maybe 1 (1 + )


isCodeAttr :: AttrInfo -> Bool
isCodeAttr (CodeAttr _ _ _ _ _) = True
isCodeAttr _ = False

readFileContents :: FilePath -> IO [Word8]
readFileContents path = BS.unpack <$> BS.readFile path

parseFileContents :: FilePath -> IO (Either String ByteCode)
parseFileContents path = do
  contents <- readFileContents path
  let parsed = parse contents
  return $ either formatParseError validateParseResult parsed

formatParseError :: (LBS.ByteString, ByteOffset, String) -> Either String ByteCode
formatParseError (_, _, msg) = Left msg

validateParseResult :: (LBS.ByteString, ByteOffset, ByteCode) -> Either String ByteCode
validateParseResult (_, _, bc) = Right bc

type OpCode = String

oc :: Instruction -> String
oc Nop = "nop"
oc AConstNull = "aconst_null"
oc IConstM1 =  "iconst_m1"
oc IConst0 = "iconst_0"
oc IConst1 = "iconst_1"

oc _ = "unknown"
