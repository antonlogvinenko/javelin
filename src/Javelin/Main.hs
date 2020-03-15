module Javelin.Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe            (runMaybeT)
import qualified Data.ByteString                as BS (readFile, unpack)
import           Data.Semigroup                       ((<>))
import           Javelin.Lib.ByteCode.ClassFile       (parseRaw)
import           Javelin.Lib.ByteCode.Data            (showByteCode)
import           Javelin.Lib.ByteCode.Stats           (stats)
import           Javelin.Runtime.Instructions         (runJVM)
import           Javelin.Interpreter.ClassPathLoading
import           Javelin.Interpreter.Loading          (deriveClass, loadClassOrArray)
import           Javelin.Interpreter.JVMApp           (runJVMApp, JVMConfig(..))
import           Javelin.Lib.Structures               hiding (long)
import           Options.Applicative
import           System.Directory
import           System.Environment
import           Text.Show.Pretty
import           Javelin.Capability.Classes

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
        Right (_, _, v) -> putStrLn $ ppShow $ deriveClass v
        Left (_, off, v) ->
            putStrLn $ "Failed to parse file " ++ path ++ ". Offset " ++ show off

loadClassPath opt bs =
    let words = BS.unpack bs
    in case parseRaw words of
        Right (_, _, v)  -> putStrLn $ showByteCode opt v
        Left (_, off, v) -> putStrLn "Failed to parse file"

loadClassWithDepsPure :: Global m => FilePath -> String -> m (Either VMError Runtime)
loadClassWithDepsPure classPath className = do
  layout <- getClassSourcesLayout classPath
  let rt = newRuntime layout
  classBytes <- getClassBytes className layout
  loadClass (ClassId BootstrapClassLoader className) rt

loadClassWithDeps :: FilePath -> String -> IO (Either VMError Runtime)
loadClassWithDeps classPath className = runJVMApp (loadClassWithDepsPure classPath className) (JVMConfig False)

data JVMOpts
    = Disasm { classFilePath :: String }
    | DisasmFull { classFilePath :: String }
    | DisasmSemantics { classFilePath :: String }
    | DisasmByteCodeStats { dirPath        :: String
                        , outputFilePath :: Maybe String }
    | LoadClassPath { classPath     :: String
                    , classFilePath :: String }
    | LoadClassWithDeps { classPath     :: String
                        , classFilePath :: String }
    | JVM { mainClass :: String
        , classPath :: String
        , silent    :: Bool
        , args      :: [String] }

javelinMain :: IO ()
javelinMain = execParser opts >>= runWithOptions
    where
    opts = info topParser $ progDesc "JVM implementation"
    topParser =
        subparser $
        command "disasm" (info disasmParser $ progDesc "Disassemble a class") <>
        command
        "disasmSemantics"
        (info disasmSemanticsParser $
            progDesc "Display class contents in a readable form") <>
        command
        "disasmFull"
        (info disasmFullParser $
            progDesc "Disassembler with traced references to constant pool") <>
        command "stats" (info disasmStatsParser $ progDesc "Bytecode statistics") <>
        command
        "classpath"
        (info loadClassPathParser $
            progDesc "Parsing classpath, traversing it and loading class bytes") <>
        command
          "loadClass"
          (info loadClassWithDepsParser $
           progDesc "Loading class with all dependencies") <>
        command "jvm" (info jvmParser $ progDesc "Run JVM")
    disasmParser = Disasm <$> strArgument (metavar "Path to class file")
    disasmFullParser = DisasmFull <$> strArgument (metavar "Path to class file")
    disasmSemanticsParser =
        DisasmSemantics <$> strArgument (metavar "Path to class file")
    disasmStatsParser =
        DisasmByteCodeStats <$> strArgument (metavar "Directory with classes") <*>
        optional (strArgument (metavar "Output file path"))
    loadClassPathParser =
        LoadClassPath <$> strArgument (metavar "JVM class path") <*>
        strArgument (metavar "Class file to load")
    loadClassWithDepsParser =
      LoadClassWithDeps <$> strArgument (metavar "JVM class path") <*>
      strArgument (metavar "Class file to load")
    jvmParser =
        JVM <$> strArgument (metavar "mainClass") <*>
        strArgument (metavar "classPath") <*>
        flag True False (long "silentMode" <> short 's') <*>
        some (strArgument (metavar "mainArguments"))

runWithOptions :: JVMOpts -> IO ()
runWithOptions jvmOpts =
    case jvmOpts of
        Disasm path -> disasmClass False path
        DisasmFull path -> disasmClass True path
        DisasmSemantics path -> disasmSemantics path
        DisasmByteCodeStats path mbOutput -> stats path mbOutput
        LoadClassPath classPath classFilePath -> do
          bb <- runJVMApp (getClassSourcesLayout classPath >>= getClassBytes classFilePath) (JVMConfig False)
          case bb of
            Left error -> print error
            Right bs -> loadClassPath False bs
        LoadClassWithDeps classPath classFilePath ->
          (loadClassWithDeps classPath classFilePath) >>= print
        JVM mainClass classPath silentMode mainArgs ->
            runJVMApp (runJVM classPath mainClass mainArgs) (JVMConfig silentMode)
