module Javelin.Runtime.Instructions where

import           Control.Monad.State.Lazy   (State, runState, state)
import           Data.Bits                  (xor, (.&.), (.|.))
import qualified Data.Map.Lazy              as Map (Map, fromList, lookup, (!))
import           Data.Word                  (Word16, Word32, Word64, Word8)
import           Javelin.Runtime.Structures
import           Javelin.Runtime.Thread
import           Javelin.Runtime.LLI.ClassPath (getClassSourcesLayout)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Array.IArray          (array)
import           Debug.Trace
import           System.Exit                (die)
import qualified Javelin.Runtime.LLI.LinkingInitializing as LI (init)
import System.IO (writeFile)
import Flow

--stack exec javelin jvm test.App /Library/Java/JavaVirtualMachines/jdk1.8.0_201.jdk/Contents/Home/jre/lib/rt.jar:main 1
--runJVM "/Library/Java/JavaVirtualMachines/jdk1.8.0_201.jdk/Contents/Home/jre/lib/rt.jar:main" "test.App" []

-- We need to start eexuting commands in main method of main class so we have to make it look like
-- someone called 'invokestatic' on main method of main class: the class is loaded, there is a frame with main class id in it etc

---- 1. enable some sort of logging

console :: Show a => String -> a -> IO ()
console x a = putStrLn $ x ++ ": " ++ (show a)

dump :: Show a => String -> a -> IO ()
dump file a = writeFile ("./logs/" ++ file) (show a)

runJVM :: String -> String -> [String] -> IO ()
runJVM classPath mainClass args =
  let main = map (\c -> if c == '.' then '/' else c) mainClass
  in do
    putStrLn ""
    putStrLn "_______ Starting JVM ________"
    console "Main class arg" mainClass
    console "Main class" main
    maybeCPLayout <- runExceptT $ getClassSourcesLayout classPath
    case maybeCPLayout of
      Left error -> console "Failed while loading class path" error
      Right cpLayout -> console "Classpath" (_classPath cpLayout) >> dump "classpath.log" cpLayout >>
        case (Map.!) (_classes cpLayout) main of
          JarFile path -> die "Not implemented yet: running JVM from a main class inside jar file"
          ClassFile path -> do
            console "Main class found in class file" path
            let classId = ClassId BootstrapClassLoader main
            mainClassInit <- runExceptT $ LI.init classId (newRuntime cpLayout)
            case mainClassInit of
              Left err -> die $ show err
              Right rt -> case createMainFrame rt classId of
                Right frame -> runThread 0 $ Thread 0 [frame] rt
                err -> die $ show err

-- todo: create frame according to description in method: locals etc
-- implement 2+2
-- print state between executions 

-- implement printing to console
createMainFrame :: Runtime -> ClassId -> Either VMError Frame
createMainFrame rt classId = createFrame rt classId (PartReference "main" "(Ljava/lang/String[];)V")

createFrame :: Runtime -> ClassId -> PartReference -> Either VMError Frame
createFrame rt classId methodReference =
  case getMethodBySignature rt classId methodReference of
    Right index -> Right $ Frame classId index (Locals $ array (0, 100) []) []
    Left err -> Left err


-- find next instruction and its argument bytes
-- handle 'no more commands' and exit
-- add auto logging of in/out of commands
runThread :: Int -> Thread -> IO ()
runThread c thread =
  if c > 1000
  then print "Exiting"
  else let (arguments, instruction) = nextInstructionLine thread
           (_, newThread) = runState (instruction arguments) thread
       in runThread (c + 1) newThread

nextInstructionLine :: Thread -> ([Word8], Instruction)
nextInstructionLine = undefined

type ArgumentsParser = [Word8] -> [Word8]

fixed :: Int -> ArgumentsParser
fixed = take

dummy :: ArgumentsParser
dummy = undefined

noarg :: ArgumentsParser
noarg = const []

-- Instructions implementation
-- Constants
nop = empty

iconst x = push (x :: JInt)

lconst x = push (x :: JLong)

fconst x = push (x :: JFloat)

dconst x = push (x :: JDouble)

aconst_null = iconst 0

iconst_m1 = iconst (-1)

iconst_0 = iconst 0

iconst_1 = iconst 1

iconst_2 = iconst 2

iconst_3 = iconst 3

iconst_4 = iconst 4

iconst_5 = iconst 5

lconst_0 = lconst 0

lconst_1 = lconst 1

fconst_0 = fconst 0

fconst_1 = fconst 1

fconst_2 = fconst 2

dconst_0 = dconst 0

dconst_1 = dconst 1

bipush = do
  byte <- arg jbyte 0
  push $ signExtend byte

sipush = do
  short <- arg jshort 0
  push $ signExtend short

ldc = undefined

ldc_w = undefined

ldc2_w = undefined

-- Loads
iloadFrom idx = do
  var <- load jint idx
  push var

iload = do
  idx <- arg jbyte 0
  iloadFrom idx

lload = undefined

fload = undefined

dload = undefined

aload = undefined

iload_0 = iloadFrom 0

iload_1 = iloadFrom 1

iload_2 = iloadFrom 2

iload_3 = iloadFrom 3

lload_0 = undefined

lload_1 = undefined

lload_2 = undefined

lload_3 = undefined

fload_0 = undefined

fload_1 = undefined

fload_2 = undefined

fload_3 = undefined

dload_0 = undefined

dload_1 = undefined

dload_2 = undefined

dload_3 = undefined

aload_0 = undefined

aload_1 = undefined

aload_2 = undefined

aload_3 = undefined

iaload = undefined

laload = undefined

faload = undefined

daload = undefined

aaload = undefined

baload = undefined

caload = undefined

saload = undefined

-- Stores
istoreAt idx = do
  op <- pop jint
  store op idx

istore = do
  idx <- arg jbyte 0
  istoreAt idx

lstore = undefined

fstore = undefined

dstore = undefined

astore = undefined

istore_0 = undefined

istore_1 = undefined

istore_2 = undefined

istore_3 = undefined

lstore_0 = undefined

lstore_1 = undefined

lstore_2 = undefined

lstore_3 = undefined

fstore_0 = undefined

fstore_1 = undefined

fstore_2 = undefined

fstore_3 = undefined

dstore_0 = undefined

dstore_1 = undefined

dstore_2 = undefined

dstore_3 = undefined

astore_0 = undefined

astore_1 = undefined

astore_2 = undefined

astore_3 = undefined

iastore = undefined

lastore = undefined

fastore = undefined

dastore = undefined

aastore = undefined

bastore = undefined

castore = undefined

sastore = undefined

-- Stack
pop1 = do
  remove

pop2 = do
  remove
  remove

dup = do
  op <- peek jraw
  push op

-- dup_x1 = do
--   [op1, op2] <- popn jraw 2
--   pushn [op1, op2, op1]

-- dup_x2 = do
--   [op1, op2, op3] <- popn jraw 3
--   pushn [op1, op3, op2, op1]

-- dup2 = do
--   [op1, op2] <- popn jraw 2
--   pushn [op2, op1, op2, op1]

-- dup2_x1 = do
--   [op1, op2, op3] <- popn jraw 3
--   pushn [op2, op1, op3, op2, op1]

-- dup2_x2 = do
--   [op1, op2, op3, op4] <- popn jraw 4
--   pushn [op2, op1, op4, op3, op2, op1]

-- swap = do
--   [op1, op2] <- popn jraw 2
--   pushn [op2, op1]

-- Math
math operandType operation = do
  op1 <- pop operandType
  op2 <- pop operandType
  push $ operation op1 op2

iadd = math jint (+)

ladd = math jlong (+)

fadd = math jfloat (+)

dadd = math jdouble (+)

isub = math jint (-)

lsub = math jlong (-)

fsub = math jfloat (-)

dsub = math jdouble (-)

imul = math jint (*)

lmul = math jlong (*)

fmul = math jfloat (*)

dmul = math jdouble (*)

idiv = math jint div

ldiv = math jlong div

fdiv = math jfloat (/)

ddiv = math jdouble (/)

irem = math jint rem

lrem = math jlong rem

frem = undefined

drem = undefined

neg operandType = do
  x <- pop operandType
  push $ -x

ineg = neg jint

lneg = neg jlong

fneg = neg jfloat

dneg = neg jdouble

ishl = undefined

lshl = undefined

ishr = undefined

lshr = undefined

iushr = undefined

lushr = undefined

iand = math jint (.&.)

land = math jlong (.&.)

ior = math jint (.|.)

lor = math jlong (.|.)

ixor = math jint xor

lxor = math jlong xor

-- wrong implementation: bytes are read from instruction not local arguments
iinc :: Instruction
iinc bytes = do
  index <- arg jbyte 0
  const <- arg jbyte 0
  var <- load jint index
  let constExt = signExtend const
  let newVar = var + constExt
  store newVar index

-- Conversions
i2l = undefined

i2f = undefined

i2d = undefined

l2i = undefined

l2f = undefined

l2d = undefined

f2i = undefined

f2l = undefined

f2d = undefined

d2i = undefined

d2l = undefined

d2f = undefined

i2b = undefined

i2c = undefined

i2s = undefined

-- Comparisons
lcmp = undefined

fcmpl = undefined

fcmpg = undefined

dcmpl = undefined

dcmpg = undefined

ifeq = undefined

ifne = undefined

iflt = undefined

ifge = undefined

ifgt = undefined

ifle = undefined

if_icmpeq = undefined

if_icmpne = undefined

if_icmplt = undefined

if_icmpge = undefined

if_icmpgt = undefined

if_icmple = undefined

if_acmpeq = undefined

if_acmpne = undefined

-- Control
goto = undefined

jsr = undefined

ret = undefined

tableswitch = undefined

lookupswitch = undefined

ireturn = undefined

lreturn = undefined

freturn = undefined

dreturn = undefined

areturn = undefined

_return = undefined

-- References
getstatic = undefined

putstatic = undefined

getfield = undefined

putfield = undefined

invokevirtual = undefined

invokespecial :: Instruction
invokespecial bytes = undefined

-- Set pc to 0
-- Create new stack frame
-- resolve the method, initialize the class
invokestatic :: Instruction
invokestatic bytes =
  state $ \t@(Thread c fs rt) ->
            let thread = Thread 0 (newFrame:fs) newRT
                newRT = undefined
                newFrame = undefined :: Frame
            in ((), thread)

invokeinterface = undefined

invokedynamic = undefined

new = undefined

newarray = undefined

anewarray = undefined

arraylength = undefined

athrow = undefined

checkcast = undefined

instanceof = undefined

monitorenter = undefined

monitorexit = undefined

-- Extended
wide :: Instruction
wide bytes = undefined

multianewarray = undefined

ifnull = undefined

ifnonnull = undefined

goto_w = undefined

jsr_w = undefined

-- Reserved
breakpoint = undefined

impdep1 = undefined

impdep2 = undefined

-- Instruction listing
tableswitchArgs :: ArgumentsParser
tableswitchArgs = undefined

lookupswitchArgs :: ArgumentsParser
lookupswitchArgs = undefined

wideArgs :: ArgumentsParser
wideArgs = undefined

multianewarrayArgs :: ArgumentsParser
multianewarrayArgs = undefined

 
instructions :: Map.Map Word8 (ArgumentsParser, Instruction)
instructions =
  Map.fromList
  -- Constants
    [ (0x00, (noarg, const nop))
    , (0x01, (noarg, const aconst_null))
    , (0x02, (noarg, const iconst_m1))
    , (0x03, (noarg, const iconst_0))
    , (0x04, (noarg, const iconst_1))
    , (0x05, (noarg, const iconst_2))
    , (0x06, (noarg, const iconst_3))
    , (0x07, (noarg, const iconst_4))
    , (0x08, (noarg, const iconst_5))
    , (0x09, (noarg, const lconst_0))
    , (0x0a, (noarg, const lconst_1))
    , (0x0b, (noarg, const fconst_0))
    , (0x0c, (noarg, const fconst_1))
    , (0x0d, (noarg, const fconst_2))
    , (0x0e, (noarg, const dconst_0))
    , (0x0f, (noarg, const dconst_1))
    , (0x10, (fixed 1, const bipush))
    , (0x11, (fixed 2, const sipush))
    , (0x12, (fixed 1, ldc))
    , (0x13, (fixed 2, ldc_w))
    , (0x14, (fixed 2, ldc2_w))
  -- Loads
    , (0x15, (fixed 1, const iload))
    , (0x16, (fixed 1, lload))
    , (0x17, (fixed 1, fload))
    , (0x18, (fixed 1, dload))
    , (0x19, (fixed 1, aload))
    , (0x1a, (noarg, const iload_0))
    , (0x1b, (noarg, const iload_1))
    , (0x1c, (noarg, const iload_2))
    , (0x1d, (noarg, const iload_3))
    , (0x1e, (noarg, lload_0))
    , (0x1f, (noarg, lload_1))
    , (0x20, (noarg, lload_2))
    , (0x21, (noarg, lload_3))
    , (0x22, (noarg, fload_0))
    , (0x23, (noarg, fload_1))
    , (0x24, (noarg, fload_2))
    , (0x25, (noarg, fload_3))
    , (0x26, (noarg, dload_0))
    , (0x27, (noarg, dload_1))
    , (0x28, (noarg, dload_2))
    , (0x29, (noarg, dload_3))
    , (0x2a, (noarg, aload_0))
    , (0x2b, (noarg, aload_1))
    , (0x2c, (noarg, aload_2))
    , (0x2d, (noarg, aload_3))
    , (0x2e, (noarg, iaload))
    , (0x2f, (noarg, laload))
    , (0x30, (noarg, faload))
    , (0x31, (noarg, daload))
    , (0x32, (noarg, aaload))
    , (0x33, (noarg, baload))
    , (0x34, (noarg, caload))
    , (0x35, (noarg, saload))
  -- Stores
    , (0x36, (fixed 1, const istore))
    , (0x37, (fixed 1, lstore))
    , (0x38, (fixed 1, fstore))
    , (0x39, (fixed 1, dstore))
    , (0x3a, (fixed 1, astore))
    , (0x3b, (noarg, istore_0))
    , (0x3c, (noarg, istore_1))
    , (0x3d, (noarg, istore_2))
    , (0x3e, (noarg, istore_3))
    , (0x3f, (noarg, lstore_0))
    , (0x40, (noarg, lstore_1))
    , (0x41, (noarg, lstore_2))
    , (0x42, (noarg, lstore_3))
    , (0x43, (noarg, fstore_0))
    , (0x44, (noarg, fstore_1))
    , (0x45, (noarg, fstore_2))
    , (0x46, (noarg, fstore_3))
    , (0x47, (noarg, dstore_0))
    , (0x48, (noarg, dstore_1))
    , (0x49, (noarg, dstore_2))
    , (0x4a, (noarg, dstore_3))
    , (0x4b, (noarg, astore_0))
    , (0x4c, (noarg, astore_1))
    , (0x4d, (noarg, astore_2))
    , (0x4e, (noarg, astore_3))
    , (0x4f, (noarg, iastore))
    , (0x50, (noarg, lastore))
    , (0x51, (noarg, fastore))
    , (0x52, (noarg, dastore))
    , (0x53, (noarg, aastore))
    , (0x54, (noarg, bastore))
    , (0x55, (noarg, castore))
    , (0x56, (noarg, sastore))
  -- Stack
    , (0x57, (noarg, const pop1))
    , (0x58, (noarg, const pop2))
    , (0x59, (noarg, const dup))
    -- , (0x5a, (noarg, const dup_x1))
    -- , (0x5b, (noarg, const dup_x2))
    -- , (0x5c, (noarg, const dup2))
    -- , (0x5d, (noarg, const dup2_x1))
    -- , (0x5e, (noarg, const dup2_x2))
    -- , (0x5f, (noarg, const swap))
  -- Math
    , (0x60, (noarg, const iadd))
    , (0x61, (noarg, const ladd))
    , (0x62, (noarg, const fadd))
    , (0x63, (noarg, const dadd))
    , (0x64, (noarg, const isub))
    , (0x65, (noarg, const lsub))
    , (0x66, (noarg, const fsub))
    , (0x67, (noarg, const dsub))
    , (0x68, (noarg, const imul))
    , (0x69, (noarg, const lmul))
    , (0x6a, (noarg, const fmul))
    , (0x6b, (noarg, const dmul))
    , (0x6c, (noarg, const idiv))
    , (0x6d, (noarg, const ldiv))
    , (0x6e, (noarg, const fdiv))
    , (0x6f, (noarg, const ddiv))
    , (0x70, (noarg, const irem))
    , (0x71, (noarg, const lrem))
    , (0x72, (noarg, frem))
    , (0x73, (noarg, drem))
    , (0x74, (noarg, const ineg))
    , (0x75, (noarg, const lneg))
    , (0x76, (noarg, const fneg))
    , (0x77, (noarg, const dneg))
    , (0x78, (noarg, ishl))
    , (0x79, (noarg, lshl))
    , (0x7a, (noarg, ishr))
    , (0x7b, (noarg, lshr))
    , (0x7c, (noarg, iushr))
    , (0x7d, (noarg, lushr))
    , (0x7e, (noarg, const iand))
    , (0x7f, (noarg, const land))
    , (0x80, (noarg, const ior))
    , (0x81, (noarg, const lor))
    , (0x82, (noarg, const ixor))
    , (0x83, (noarg, const lxor))
    , (0x84, (fixed 2, iinc))
  -- Conversions
    , (0x85, (noarg, i2l))
    , (0x86, (noarg, i2f))
    , (0x87, (noarg, i2d))
    , (0x88, (noarg, l2i))
    , (0x89, (noarg, l2f))
    , (0x8a, (noarg, l2d))
    , (0x8b, (noarg, f2i))
    , (0x8c, (noarg, f2l))
    , (0x8d, (noarg, f2d))
    , (0x8e, (noarg, d2i))
    , (0x8f, (noarg, d2l))
    , (0x90, (noarg, d2f))
    , (0x91, (noarg, i2b))
    , (0x92, (noarg, i2c))
    , (0x93, (noarg, i2s))
  -- Comparisons
    , (0x94, (noarg, lcmp))
    , (0x95, (noarg, fcmpl))
    , (0x96, (noarg, fcmpg))
    , (0x97, (noarg, dcmpl))
    , (0x98, (noarg, dcmpg))
    , (0x99, (fixed 2, ifeq))
    , (0x9a, (fixed 2, ifne))
    , (0x9b, (fixed 2, iflt))
    , (0x9c, (fixed 2, ifge))
    , (0x9d, (fixed 2, ifgt))
    , (0x9e, (fixed 2, ifle))
    , (0x9f, (fixed 2, if_icmpeq))
    , (0xa0, (fixed 2, if_icmpne))
    , (0xa1, (fixed 2, if_icmplt))
    , (0xa2, (fixed 2, if_icmpge))
    , (0xa3, (fixed 2, if_icmpgt))
    , (0xa4, (fixed 2, if_icmple))
    , (0xa5, (fixed 2, if_acmpeq))
    , (0xa6, (fixed 2, if_acmpne))
  -- Control
    , (0xa7, (fixed 2, goto))
    , (0xa8, (fixed 2, jsr))
    , (0xa9, (fixed 1, ret))
    , (0xaa, (tableswitchArgs, tableswitch))
    , (0xab, (lookupswitchArgs, lookupswitch))
    , (0xac, (noarg, ireturn))
    , (0xad, (noarg, lreturn))
    , (0xae, (noarg, freturn))
    , (0xaf, (noarg, dreturn))
    , (0xb0, (noarg, areturn))
    , (0xb1, (noarg, _return))
  -- References
    , (0xb2, (fixed 2, getstatic))
    , (0xb3, (fixed 2, putstatic))
    , (0xb4, (fixed 2, getfield))
    , (0xb5, (fixed 2, putfield))
    , (0xb6, (fixed 2, invokevirtual))
    , (0xb7, (fixed 2, invokespecial))
    , (0xb8, (fixed 2, invokestatic))
    , (0xb9, (fixed 4, invokeinterface))
    , (0xba, (fixed 4, invokedynamic))
    , (0xbb, (fixed 2, new))
    , (0xbc, (fixed 1, newarray))
    , (0xbd, (fixed 2, anewarray))
    , (0xbe, (noarg, arraylength))
    , (0xbf, (noarg, athrow))
    , (0xc0, (fixed 2, checkcast))
    , (0xc1, (fixed 2, instanceof))
    , (0xc2, (noarg, monitorenter))
    , (0xc3, (noarg, monitorexit))
  -- Extended
    , (0xc4, (wideArgs, wide))
    , (0xc5, (multianewarrayArgs, multianewarray))
    , (0xc6, (fixed 2, ifnull))
    , (0xc7, (fixed 2, ifnonnull))
    , (0xc8, (fixed 4, goto_w))
    , (0xc9, (fixed 4, jsr_w))
  -- Reserved
    , (0xca, (noarg, breakpoint))
    , (0xfe, (noarg, impdep1))
    , (0xff, (noarg, impdep2))
    ]
