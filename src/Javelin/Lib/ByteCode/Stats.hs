{-# LANGUAGE BangPatterns #-}

module Javelin.Lib.ByteCode.Stats where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Except as Except
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Ord as Ord
import qualified System.Directory as Dir
import qualified Text.Printf as Printf
import qualified Data.Bifunctor as BF

import Javelin.Lib.ByteCode.ClassFile
import Javelin.Lib.ByteCode.Data

listDir :: FilePath -> IO [FilePath]
listDir path = do
  isDirectory <- Dir.doesDirectoryExist path
  if not isDirectory
    then return [path]
    else do
      contents <- Dir.getDirectoryContents path
      let files =
            map ((path ++ "/") ++) . filter (`notElem` [".", ".."]) $ contents
      concat <$> mapM listDir files

getStats :: FilePath -> IO (Either String (Map.Map String Integer))
getStats path = do
  filePaths <- listDir path
  Except.runExceptT $ calcFreqs mempty filePaths

stats :: FilePath -> Maybe FilePath -> IO ()
stats path output = do
  result <- getStats path
  case result of
    Left msg -> putStrLn msg
    Right freqs ->
      let list =
            List.sortOn (Ord.Down . snd) $
            Map.foldrWithKey (\k v list -> (k, v) : list) [] freqs
          amount = fromIntegral $ foldl (\c (k, v) -> c + v) 0 list
          normalize x = (fromIntegral (100 * x) / amount) :: Double
          normalized = map (BF.second normalize) list
          formatted = map (BF.second (Printf.printf "%.4f%%")) normalized
       in do maybe mempty (textile formatted) output
             printConsole formatted

printConsole :: [(OpCode, String)] -> IO ()
printConsole freqs = putStrLn $ concatMap (uncurry (Printf.printf "%s: %s\n")) freqs

textile :: [(OpCode, String)] -> FilePath -> IO ()
textile freqs path = do
  writeFile path "| OpCpde | Frequency |\n"
  appendFile path $ concatMap (uncurry (Printf.printf "| %s | %s |\n")) freqs

calcFreqs ::
     Map.Map String Integer
  -> [FilePath]
  -> Except.ExceptT String IO (Map.Map String Integer)
calcFreqs accum [] = return accum
calcFreqs !accum (f:fs) = do
  bytecode <- parseFileContents f
  calcFreqs (addFreq accum bytecode) fs

addFreq :: Map.Map OpCode Integer -> ByteCode -> Map.Map OpCode Integer
addFreq freq bc =
  let opCodes = do
        method <- methods $ body bc
        attr <- methodAttrs method
        Monad.guard (isCodeAttr attr)
        instruction <- code attr
        return $ oc instruction
   in composeFreqs freq opCodes

composeFreqs :: Map.Map OpCode Integer -> [OpCode] -> Map.Map OpCode Integer
composeFreqs x [] = x
composeFreqs x (c:cs) = composeFreqs (Map.alter statsAlter c x) cs
  where
    statsAlter = Just . maybe 1 (1 +)

isCodeAttr :: AttrInfo -> Bool
isCodeAttr CodeAttr {} = True
isCodeAttr _ = False

parseFileContents :: FilePath -> Except.ExceptT String IO ByteCode
parseFileContents path =
  Except.ExceptT $
  either (addPath path) Right <$> parse <$> BS.unpack <$> BS.readFile path

addPath :: String -> String -> Either String ByteCode
addPath path msg = Left $ "Failed parsing file " ++ path ++ ". " ++ msg

type OpCode = String

oc :: Instruction -> String
-- Constants
oc Nop = "nop"
oc AConstNull = "aconst_null"
oc IConstM1 = "iconst_m1"
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
oc ALoad0 = "aload_0"
oc ALoad1 = "aload_1"
oc ALoad2 = "aload_2"
oc ALoad3 = "aload_3"
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
oc AStore0 = "astore_0"
oc AStore1 = "astore_1"
oc AStore2 = "astore_2"
oc AStore3 = "astore_3"
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
oc IAdd = "iadd"
oc LAdd = "ladd"
oc FAdd = "fadd"
oc DAdd = "dadd"
oc ISub = "isub"
oc LSub = "lsub"
oc FSub = "fsub"
oc DSub = "dsub"
oc IMul = "imul"
oc LMul = "lmul"
oc FMul = "fmul"
oc DMul = "dmul"
oc IDiv = "idiv"
oc LDiv = "ldiv"
oc FDiv = "fdiv"
oc DDiv = "ddiv"
oc IRem = "irem"
oc LRem = "lrem"
oc FRem = "frem"
oc DRem = "drem"
oc INeg = "ineg"
oc LNeg = "lneg"
oc FNeg = "fneg"
oc DNeg = "dneg"
oc IShl = "ishl"
oc LShl = "lshl"
oc IShr = "ishr"
oc LShr = "lshr"
oc IUshr = "iushr"
oc LUshr = "lushr"
oc IAnd = "iand"
oc LAnd = "land"
oc IOr = "ior"
oc LOr = "lor"
oc IXor = "ixor"
oc LXor = "lxor"
oc (IInc _ _) = "iinc"
-- Conversions
oc I2L = "i2l"
oc I2F = "i2f"
oc I2D = "i2d"
oc L2I = "l2i"
oc L2F = "l2f"
oc L2D = "l2d"
oc F2I = "f2i"
oc F2L = "f2l"
oc F2D = "f2d"
oc D2I = "d2i"
oc D2L = "d2l"
oc D2F = "d2f"
oc I2B = "i2b"
oc I2C = "i2c"
oc I2S = "i2s"
-- Comparisons
oc LCmp = "lcmp"
oc FCmpL = "fcmpl"
oc FCmpG = "fcmpg"
oc DCmpL = "dcmpl"
oc DCmpG = "dcmpg"
oc (IfEq _) = "ifeq"
oc (IfNe _) = "ifne"
oc (IfLt _) = "iflt"
oc (IfGe _) = "ifge"
oc (IfGt _) = "ifgt"
oc (IfLe _) = "ifle"
oc (IfICmpEq _) = "if_icmpeq"
oc (IfICmpNe _) = "if_icmpne"
oc (IfICmpLt _) = "if_icmplt"
oc (IfICmpGe _) = "if_icmpge"
oc (IfICmpGt _) = "if_icmpgt"
oc (IfICmpLe _) = "if_icmple"
oc (IfACmpEq _) = "if_acmpeq"
oc (IfACmpNe _) = "if_acmpne"
-- Control
oc (Goto _) = "goto"
oc (Jsr _) = "jsr"
oc (Ret _) = "ret"
oc (TableSwitch {}) = "tableswitch"
oc (LookupSwitch _ _) = "lookupswitch"
oc IReturn = "ireturn"
oc LReturn = "lreturn"
oc FReturn = "freturn"
oc DReturn = "dreturn"
oc AReturn = "areturn"
oc Return = "return"
-- References
oc (GetStatic _) = "getstatic"
oc (PutStatic _) = "putstatic"
oc (GetField _) = "getfield"
oc (PutField _) = "putfield"
oc (InvokeVirtual _) = "invokevirtual"
oc (InvokeSpecial _) = "invokespecial"
oc (InvokeStatic _) = "invokestatic"
oc (InvokeInterface _ _) = "invokeinterface"
oc (InvokeDynamic _ _) = "invokedynamic"
oc (New_ _) = "new"
oc (NewArray _) = "newarray"
oc (ANewArray _) = "anewarray"
oc ArrayLength = "arraylength"
oc AThrow = "athrow"
oc (CheckCast _) = "checkcast"
oc (InstanceOf_ _) = "instanceof"
oc MonitorEnter = "monitorenter"
oc MonitorExit = "monitorexit"
-- Extended
oc (WideIInc _ _) = "wide_iinc"
oc (WideILoad _) = "wide_iload"
oc (WideFLoad _) = "wide_fload"
oc (WideALoad _) = "wide_aload"
oc (WideLLoad _) = "wide_lload"
oc (WideDLoad _) = "wide_dload"
oc (WideIStore _) = "wide_istore"
oc (WideFStore _) = "wide_fstore"
oc (WideAStore _) = "wide_astore"
oc (WideLStore _) = "wide_lstore"
oc (WideDStore _) = "wide_dstore"
oc (WideRet _) = "wide_ret"
oc (MultiANewArray _ _) = "multianewarray"
oc (IfNull _) = "ifnull"
oc (IfNotNull _) = "ifnotnull"
oc (GotoW _) = "goto_w"
oc (JsrW _) = "jsr_w"
-- Reserved
oc BreakPoint = "breakpoint"
oc ImDep1 = "imdep1"
oc ImDep2 = "imdep2"
-- oc _ = "unknown"
