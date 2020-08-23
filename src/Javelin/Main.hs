module Javelin.Main
  ( javelinMain
  ) where

import qualified Data.ByteString as BS
import qualified Text.Show.Pretty as Pretty
import qualified Options.Applicative as Opt

import Javelin.Capability.Classes
import Javelin.Interpreter.ClassPathLoading ()
import Javelin.Interpreter.JVMApp (JVMConfig(..), runJVMApp)
import Javelin.Interpreter.Loading (deriveClass)
import Javelin.Interpreter.Logging ()
import Javelin.Interpreter.StdIO ()
import Javelin.Interpreter.Termination ()
import Javelin.Lib.ByteCode.ClassFile (parseRaw)
import Javelin.Lib.ByteCode.Data (showByteCode)
import Javelin.Lib.ByteCode.Stats (stats)
import Javelin.Lib.Structures hiding (long)
import Javelin.Runtime.Instructions (runJVM)

disasmClass :: Bool -> FilePath -> IO ()
disasmClass opt path = do
  bytestring <- BS.readFile path
  let words = BS.unpack bytestring
  case parseRaw words of
    Right (_, _, v) -> putStrLn $ showByteCode opt v
    Left (_, off, v) ->
      putStrLn $ "Failed to parse file " ++ path ++ ". Offset " ++ show off

disasmSemantics path = do
  bytestring <- BS.readFile path
  let words = BS.unpack bytestring
  case parseRaw words of
    Right (_, _, v) -> putStrLn $ Pretty.ppShow $ deriveClass v
    Left (_, off, v) ->
      putStrLn $ "Failed to parse file " ++ path ++ ". Offset " ++ show off

loadClassPath opt bs =
  let words = BS.unpack bs
   in case parseRaw words of
        Right (_, _, v) -> putStrLn $ showByteCode opt v
        Left (_, off, v) -> putStrLn "Failed to parse file"

loadClassWithDepsPure ::
     Global m => FilePath -> String -> m (Either VMError Runtime)
loadClassWithDepsPure classPath className = do
  layout <- getClassSourcesLayout classPath
  let rt = newRuntime layout
  classBytes <- getClassBytes className layout
  loadClass (ClassId BootstrapClassLoader className) rt

loadClassWithDeps :: FilePath -> String -> IO (Either VMError Runtime)
loadClassWithDeps classPath className =
  runJVMApp (loadClassWithDepsPure classPath className) (JVMConfig False)

data JVMOpts
  = Disasm
      { classFilePath :: String
      }
  | DisasmFull
      { classFilePath :: String
      }
  | DisasmSemantics
      { classFilePath :: String
      }
  | DisasmByteCodeStats
      { dirPath :: String
      , outputFilePath :: Maybe String
      }
  | LoadClassPath
      { classPath :: String
      , classFilePath :: String
      }
  | LoadClassWithDeps
      { classPath :: String
      , classFilePath :: String
      }
  | JVM
      { mainClass :: String
      , classPath :: String
      , loggingMode :: Bool
      , args :: [String]
      }

javelinMain :: IO ()
javelinMain = Opt.execParser opts >>= runWithOptions
  where
    opts = Opt.info topParser $ Opt.progDesc "JVM implementation"
    topParser =
      Opt.subparser $
      Opt.command "disasm" (Opt.info disasmParser $ Opt.progDesc "Disassemble a class") <>
      Opt.command
        "disasmSemantics"
        (Opt.info disasmSemanticsParser $
         Opt.progDesc "Display class contents in a readable form") <>
      Opt.command
        "disasmFull"
        (Opt.info disasmFullParser $
         Opt.progDesc "Disassembler with traced references to constant pool") <>
      Opt.command "stats" (Opt.info disasmStatsParser $ Opt.progDesc "Bytecode statistics") <>
      Opt.command
        "classpath"
        (Opt.info loadClassPathParser $
         Opt.progDesc "Parsing classpath, traversing it and loading class bytes") <>
      Opt.command
        "loadClass"
        (Opt.info loadClassWithDepsParser $
         Opt.progDesc "Loading class with all dependencies") <>
      Opt.command "jvm" (Opt.info jvmParser $ Opt.progDesc "Run JVM")
    disasmParser = Disasm <$> Opt.strArgument (Opt.metavar "Path to class file")
    disasmFullParser = DisasmFull <$> Opt.strArgument (Opt.metavar "Path to class file")
    disasmSemanticsParser =
      DisasmSemantics <$> Opt.strArgument (Opt.metavar "Path to class file")
    disasmStatsParser =
      DisasmByteCodeStats <$> Opt.strArgument (Opt.metavar "Directory with classes") <*>
      Opt.optional (Opt.strArgument (Opt.metavar "Output file path"))
    loadClassPathParser =
      LoadClassPath <$> Opt.strArgument (Opt.metavar "JVM class path") <*>
      Opt.strArgument (Opt.metavar "Class file to load")
    loadClassWithDepsParser =
      LoadClassWithDeps <$> Opt.strArgument (Opt.metavar "JVM class path") <*>
      Opt.strArgument (Opt.metavar "Class file to load")
    jvmParser =
      JVM <$> Opt.strArgument (Opt.metavar "mainClass") <*>
      Opt.strArgument (Opt.metavar "classPath") <*>
      Opt.flag False True (Opt.long "loggingMode" <> Opt.short 'l') <*>
      Opt.some (Opt.strArgument (Opt.metavar "mainArguments"))

runWithOptions :: JVMOpts -> IO ()
runWithOptions jvmOpts =
  case jvmOpts of
    Disasm path -> disasmClass False path
    DisasmFull path -> disasmClass True path
    DisasmSemantics path -> disasmSemantics path
    DisasmByteCodeStats path mbOutput -> stats path mbOutput
    LoadClassPath classPath classFilePath -> do
      bb <-
        runJVMApp
          (getClassSourcesLayout classPath >>= getClassBytes classFilePath)
          (JVMConfig False)
      case bb of
        Left error -> print error
        Right bs -> loadClassPath False bs
    LoadClassWithDeps classPath classFilePath ->
      loadClassWithDeps classPath classFilePath >>= print
    JVM mainClass classPath loggingMode mainArgs ->
      runJVMApp (runJVM classPath mainClass mainArgs) (JVMConfig loggingMode)
