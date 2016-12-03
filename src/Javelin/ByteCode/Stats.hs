{-# LANGUAGE BangPatterns #-}

module Javelin.ByteCode.Stats
where

import Javelin.ByteCode.ClassFile (parse)
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import System.Directory
import Data.Word (Word8)
import Data.Binary.Get
import Javelin.ByteCode.Data
import Data.Foldable
import Data.List (sortOn)
import Data.Ord (Down(..))
import Text.Printf
import System.IO (writeFile, appendFile)

import qualified Data.ByteString.Lazy as LBS (ByteString)
import qualified Data.ByteString as BS (unpack, readFile)


stats :: FilePath -> Maybe FilePath -> IO ()
stats path output = do
  files <- getDirectoryContents path
  let filePaths = map ((path ++ "/") ++) .filter (`notElem` [".", ".."]) $ files
  result <- runExceptT $ calcFreqs mempty filePaths
  case result of
    Left msg -> putStrLn msg
    Right freqs ->
      let list = sortOn (Down . snd) $ Map.foldrWithKey (\k v list -> (k, v) : list) [] freqs
          amount = fromIntegral $ foldl (\c (k, v) -> c + v) 0 list 
          normalize x = ((fromIntegral $ 100 * x) / amount) :: Double
          normalized = map (\(k, v) -> (k, normalize v)) list
          formatted = map (\(k, v) -> (k, printf "%.2f" v)) normalized
      in do
        maybe mempty (textile formatted) output
        printConsole formatted

printConsole :: [(OpCode, String)] -> IO ()
printConsole freqs = putStrLn $ concatMap (\(k, v) -> printf "%s: %s%%\n" k v) freqs

textile :: [(OpCode, String)] -> FilePath -> IO ()
textile freqs path = do
  writeFile path "| OpCpde | Frequency |\n"
  appendFile path $ concatMap (\(k, v) -> printf "| %s | %s |\n" k v) freqs

calcFreqs :: Map.Map String Integer -> [FilePath] -> ExceptT String IO (Map.Map String Integer)
calcFreqs accum [] = return $ accum
calcFreqs !accum (f:fs) = do
  bytecode <- parseFileContents f
  calcFreqs (addFreq accum bytecode) fs

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

readFileContents :: FilePath -> ExceptT String IO [Word8]
readFileContents path = liftIO $ BS.unpack <$> BS.readFile path

parseFileContents :: FilePath -> ExceptT String IO ByteCode
parseFileContents path = do
  contents <- readFileContents path
  let parsed = parse contents
  ExceptT $ return $ either formatParseError validateParseResult parsed

formatParseError :: (LBS.ByteString, ByteOffset, String) -> Either String ByteCode
formatParseError (_, _, msg) = Left msg

validateParseResult :: (LBS.ByteString, ByteOffset, ByteCode) -> Either String ByteCode
validateParseResult (_, _, bc) = Right bc

type OpCode = String

oc :: Instruction -> String
-- Constants
oc Nop = "nop"
oc AConstNull = "aconst_null"
oc IConstM1 =  "iconst_m1"
oc IConst0 = "iconst_0"
oc IConst1 = "iconst_1"
oc IConst2 = "iconst_2"
oc IConst3 = "iconst_3"
oc IConst4 = "iconst_4"
oc IConst5 = "iconst_5"
oc LConst0 = "lconst_0"
oc LConst1 = "lconst_1"
oc FConst0 = "fconst_0"
oc FConst1 = "fconst_1"
oc FConst2 = "fconst_2"
oc DConst0 = "dconst_0"
oc DConst1 = "dconst_1"
oc (BiPush _) = "bipush"
oc (SiPush _) = "sipush"
oc (Ldc _) = "ldc"
oc (LdcW _) = "ldc_w"
oc (Ldc2W _) = "ldc2_w"

-- Loads
oc (ILoad _) = "iload"
oc (LLoad _) = "lload"
oc (FLoad _) = "fload"
oc (DLoad _) = "dload"
oc (ALoad _) = "aload"
oc ILoad0 = "iload_0"
oc ILoad1 = "iload_1"
oc ILoad2 = "iload_2"
oc ILoad3 = "iload_3"
oc LLoad0 = "lload_0"
oc LLoad1 = "lload_1"
oc LLoad2 = "lload_2"
oc LLoad3 = "lload_3"
oc FLoad0 = "fload_0"
oc FLoad1 = "fload_1"
oc FLoad2 = "fload_2"
oc FLoad3 = "fload_3"
oc DLoad0 = "dload_0"
oc DLoad1 = "dload_1"
oc DLoad2 = "dload_2"
oc DLoad3 = "dload_3"
oc IaLoad = "iaload"
oc LaLoad = "laload"
oc FaLoad = "faload"
oc DaLoad = "daload"
oc AaLoad = "aaload"
oc BaLoad = "baload"
oc CaLoad = "caload"
oc SaLoad = "saload"

-- Stores
oc (IStore _) = "istore"
oc (LStore _) = "lstore"
oc (FStore _) = "fstore"
oc (DStore _) = "dstore"
oc (AStore _) = "astore"
oc IStore0 = "istore_0"
oc IStore1 = "istore_1"
oc IStore2 = "istore_2"
oc IStore3 = "istore_3"
oc LStore0 = "lstore_0"
oc LStore1 = "lstore_1"
oc LStore2 = "lstore_2"
oc LStore3 = "lstore_3"
oc FStore0 = "fstore_0"
oc FStore1 = "fstore_1"
oc FStore2 = "fstore_2"
oc FStore3 = "fstore_3"
oc DStore0 = "dstore_0"
oc DStore1 = "dstore_1"
oc DStore2 = "dstore_2"
oc DStore3 = "dstore_3"
oc IaStore = "iastore"
oc LaStore = "lastore"
oc FaStore = "fastore"
oc DaStore = "dastore"
oc AaStore = "aastore"
oc BaStore = "bastore"
oc CaStore = "castore"
oc SaStore = "sastore"

-- Stack
oc Pop = "pop"
oc Pop2 = "pop2"
oc Dup = "dup"
oc DupX1 = "dup_x1"
oc DupX2 = "dup_x2"
oc Dup2 = "dup2"
oc Dup2X1 = "dup2_x1"
oc Dup2X2 = "dup2_x2"
oc Swap = "swap"

-- Math

oc _ = "unknown"
