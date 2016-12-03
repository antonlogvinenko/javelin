{-# LANGUAGE BangPatterns #-}

module Javelin.ByteCode.Stats
where

import Javelin.ByteCode.ClassFile (parse)
import qualified Data.Map.Strict as Map
import Control.Monad
import System.Directory
import Data.Word (Word8)
import Data.Binary.Get
import Javelin.ByteCode.Data
import Data.Foldable

import qualified Data.ByteString.Lazy as LBS (ByteString)
import qualified Data.ByteString as BS (unpack, readFile)


stats :: FilePath -> IO String
stats path = do
  files <- getDirectoryContents path
  let filePaths = map (path ++) .filter (`notElem` [".", ".."]) $ files
  freqs <- calcFreqs mempty filePaths
  return $ prettyPrint freqs

prettyPrint :: Either String (Map.Map String Integer) -> String
prettyPrint = show -- todo: format

calcFreqs :: Map.Map String Integer -> [FilePath] -> IO (Either String (Map.Map String Integer))
calcFreqs accum [] = return $ Right accum
calcFreqs !accum (f:fs) = do
  parsed <- parseFileContents f
  case parsed of
    Left msg -> return $ Left msg
    Right bc -> calcFreqs (addFreq accum bc) fs

addFreq :: Map.Map OpCode Integer -> ByteCode -> Map.Map OpCode Integer
addFreq freq bc = let opCodes = do
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

-- all instructions
-- pretty printing
-- error messages
