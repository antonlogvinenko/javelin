module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe     (runMaybeT)
import qualified Data.ByteString               as BS (readFile, unpack)
import           Javelin.ByteCode.ClassFile    (parseRaw)
import           Javelin.ByteCode.Data         (showByteCode)
import           Javelin.ByteCode.Stats        (stats)
import           Javelin.Runtime.LLI.ClassPath
import           Javelin.Runtime.LLI.Loading   (deriveClass, load)
import           Javelin.Runtime.Structures
import           Javelin.Runtime.Thread        (runJVM)
import           System.Directory
import           System.Environment
import           Text.Show.Pretty
import           Options.Applicative
import           Data.Semigroup ((<>))


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
       Left (_, off, v) -> putStrLn $ "Failed to parse file"

loadClassWithDeps :: FilePath -> String -> ExceptT VMError IO Runtime
loadClassWithDeps classPath className = do
  layout <- getClassSourcesLayout classPath
  let rt = newRuntime layout
  classBytes <- getClassBytes className layout
  load (ClassId BootstrapClassLoader className) rt

data JVMOpts =
  Disasm { classFilePath :: String }
  | DisasmFull { classFilePath :: String }
  | DisasmSemantics { classFilePath :: String }
  | DisasmByteCodeStats { dirPath :: String, outputFilePath :: Maybe String }
  | LoadClassPath { classPath :: String, classFilePath :: String }
  | LoadClassWithDeps { classPath :: String, classFilePath :: String }
  | JVM { mainClass :: String, classPath :: String, args :: [String] }

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts = info topParser (progDesc "JVM implementation")
    topParser = subparser $
                command "disasm" (info disasmParser $ progDesc "Disassemble a class")
                <> command "disasmSemantics" (info disasmSemanticsParser $ progDesc "Display class contents in a readable form")
                <> command "disasmFull" (info disasmFullParser $ progDesc "Disassembler with traced references to constant pool")
                <> command "stats" (info disasmStatsParser $ progDesc "Bytecode statistics")
                <> command "classpath" (info loadClassPathParser $ progDesc "Parsing classpath, traversing it and loading class bytes")
                <> command "loadClass" (info loadClassWithDepsParser $ progDesc "Loading class with all dependencies")
                <> command "jvm" (info jvmParser $ progDesc "Run JVM")
    disasmParser = Disasm <$> strArgument (metavar "Path to class file")
    disasmFullParser = DisasmFull <$> strArgument (metavar "Path to class file")
    disasmSemanticsParser = DisasmSemantics <$> strArgument (metavar "Path to class file")
    disasmStatsParser = DisasmByteCodeStats <$> strArgument (metavar "Directory with classes") <*> (optional $ strArgument (metavar "Output file path"))
    loadClassPathParser = LoadClassPath <$> strArgument (metavar "JVM class path") <*> strArgument (metavar "Class file to load")
    loadClassWithDepsParser = LoadClassWithDeps <$> strArgument (metavar "JVM class path") <*> strArgument (metavar "Class file to load")
    jvmParser = JVM <$> strArgument (metavar "Main class") <*> strArgument (metavar "Class path") <*> (some $ strArgument (metavar "JVM arguments"))

runWithOptions :: JVMOpts -> IO ()
runWithOptions jvmOpts = case jvmOpts of
  Disasm path -> disasmClass False path
  DisasmFull path -> disasmClass True path
  DisasmSemantics path -> disasmSemantics path
  DisasmByteCodeStats path mbOutput -> stats path mbOutput
  LoadClassPath classPath classFilePath -> let bb =
                                                 runExceptT $ do
                                                 layout <- getClassSourcesLayout classPath
                                                 classBytes <- getClassBytes classFilePath layout
                                                 lift $ loadClassPath False classBytes
                                           in bb >>= print
  LoadClassWithDeps classPath classFilePath -> (runExceptT $ loadClassWithDeps classPath classFilePath) >>= print
  JVM mainClass classPath mainArgs  -> runJVM classPath mainClass mainArgs >>= print
