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
import           Javelin.ByteCode.Data      (Instruction(..))
import           System.IO                  (writeFile)
import           Flow

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
                Right frame -> runThread 0 $ Thread [frame] rt
                err -> die $ show err

-- global plan:
-- 1. create a small snippit
-- 2. do everything for a single snippet: make it work
-- 3. write an acceptance test for the working snippet
-- 4. write unit test for new code
-- 6. do all similar functionality (similar commands)
-- 7. new acceptance & unit tests for 6
-- 8. required updates of testing infrastructure
-- goto 1: next snippet

-- current snippet:
-- int a = 2;
-- int b = 3;
-- int c = a + b;
-- System.out.println(c);

-- todo:
-- 1. implement: getstatic, invokevirtual
-- 2. see what other instructions are required
-- 3. make it work!
-- 4. print state between executions: command + stack/variables content
-- 5. testing: unit + acceptance

-- todo finish passing arguments -- but first finish 'currentFrame'
createMainFrame :: Runtime -> ClassId -> Either VMError Frame
createMainFrame rt classId = createFrame rt classId (PartReference "main" "(Ljava/lang/String[];)V")

-- todo finish: stack depth, local args size, arguments size
createFrame :: Runtime -> ClassId -> PartReference -> Either VMError Frame
createFrame rt classId methodReference =
  case getMethodBySignature rt classId methodReference of
    Right (index, method) -> let locals = Locals $ array (0, (fromIntegral $ localsSize method) - 1) []
                             in Right $ Frame 0 classId index locals  []
    Left err -> Left err


runThread :: Int -> Thread -> IO ()
runThread c thread
  | c > 100 = print "Exiting abnormally"
  | null $ frames thread = print "Exiting"
  | otherwise =
      case nextInstructionLine thread of
        Right instruction -> let execution = execute instruction
                                 (_, newThread) = runState execution thread
                             in runThread (c + 1) (incrementInstructionCounter newThread)
        Left error -> print error

incrementInstructionCounter :: Thread -> Thread
incrementInstructionCounter t@Thread{frames=(f@Frame{pc=pc}:fs)} = t{frames=f{pc=pc+1}:fs}

nextInstructionLine :: Thread -> Either VMError Instruction
nextInstructionLine Thread{frames=(Frame{pc=pc,
                                         currentClass=classId,
                                         currentMethod=methodIndex}):_,
                           runtime=rt} = do
  method <- getMethodByIndex rt classId methodIndex
  return $ instructions method !! pc

execute :: Instruction -> InstructionExecution
execute Nop = empty
-- push const <i> to stack
execute IConstM1 = iconst (-1)
execute IConst0 = iconst 0
execute IConst1 = iconst 1
execute IConst2 = iconst 2
execute IConst3 = iconst 3
execute IConst4 = iconst 4
execute IConst5 = iconst 5
-- pop int from stack, put to <i> local variable
execute IStore0 = iPopAndStoreAt 0
execute IStore1 = iPopAndStoreAt 1
execute IStore2 = iPopAndStoreAt 2
execute IStore3 = iPopAndStoreAt 3
execute (IStore localId) = iPopAndStoreAt localId
-- read int from local var <i>, push it to stack
execute (ILoad local)  = iLoadAndPushAt local
execute ILoad0 = iLoadAndPushAt 0
execute ILoad1 = iLoadAndPushAt 1
execute ILoad2 = iLoadAndPushAt 2
execute ILoad3 = iLoadAndPushAt 3
execute IAdd = do
  arg1 <- pop jint
  arg2 <- pop jint
  push $ arg1 + arg2
execute Return = dropTopFrame
-- get constant from pool
-- resolve field -> resolve class -> load class
-- init class
execute (GetStatic index) = state $ \t@Thread{runtime=rt} ->
  let pool = undefined
      fieldReference = undefined
  in do
    rt <- undefined --resolve field
    rt <- undefined --init class
    return undefined --add VMError and IO to ThreadIntruction
execute (InvokeStatic index) = undefined

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
  idx <- arg jLocalRef 0
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

istore = undefined

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
iinc :: InstructionExecution
iinc = do
  index <- arg jLocalRef 0
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

invokespecial :: InstructionExecution
invokespecial = undefined

-- Set pc to 0
-- Create new stack frame
-- resolve the method, initialize the class
invokestatic :: InstructionExecution
invokestatic =
  state $ \t@(Thread fs rt) ->
            let thread = Thread (newFrame:fs) newRT
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
wide :: InstructionExecution
wide = undefined

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
-- This mapping is not and will not ever be used directly
-- Remove once mapping between commands and their execution is finished in 'executeCommand' function
instructionSet :: Map.Map Word8 InstructionExecution
instructionSet =
  Map.fromList
  -- Constants
    [ (0x00, nop)
    , (0x01, aconst_null)
    , (0x02, iconst_m1)
    , (0x03, iconst_0)
    , (0x04, iconst_1)
    , (0x05, iconst_2)
    , (0x06, iconst_3)
    , (0x07, iconst_4)
    , (0x08, iconst_5)
    , (0x09, lconst_0)
    , (0x0a, lconst_1)
    , (0x0b, fconst_0)
    , (0x0c, fconst_1)
    , (0x0d, fconst_2)
    , (0x0e, dconst_0)
    , (0x0f, dconst_1)
    , (0x10, bipush)
    , (0x11, sipush)
    , (0x12, ldc)
    , (0x13, ldc_w)
    , (0x14, ldc2_w)
  -- -- Loads
    , (0x15, iload)
    , (0x16, lload)
    , (0x17, fload)
    , (0x18, dload)
    , (0x19, aload)
    , (0x1a,  iload_0)
    , (0x1b, iload_1)
    , (0x1c, iload_2)
    , (0x1d, iload_3)
    , (0x1e, lload_0)
    , (0x1f, lload_1)
    , (0x20, lload_2)
    , (0x21, lload_3)
    , (0x22, fload_0)
    , (0x23, fload_1)
    , (0x24, fload_2)
    , (0x25, fload_3)
    , (0x26, dload_0)
    , (0x27, dload_1)
    , (0x28, dload_2)
    , (0x29, dload_3)
    , (0x2a, aload_0)
    , (0x2b, aload_1)
    , (0x2c, aload_2)
    , (0x2d, aload_3)
    , (0x2e, iaload)
    , (0x2f, laload)
    , (0x30, faload)
    , (0x31, daload)
    , (0x32, aaload)
    , (0x33, baload)
    , (0x34, caload)
    , (0x35, saload)
  -- -- Stores
    , (0x36, istore)
    , (0x37, lstore)
    , (0x38, fstore)
    , (0x39, dstore)
    , (0x3a, astore)
    , (0x3b, istore_0)
    , (0x3c, istore_1)
    , (0x3d, istore_2)
    , (0x3e, istore_3)
    , (0x3f, lstore_0)
    , (0x40, lstore_1)
    , (0x41, lstore_2)
    , (0x42, lstore_3)
    , (0x43, fstore_0)
    , (0x44, fstore_1)
    , (0x45, fstore_2)
    , (0x46, fstore_3)
    , (0x47, dstore_0)
    , (0x48, dstore_1)
    , (0x49, dstore_2)
    , (0x4a, dstore_3)
    , (0x4b, astore_0)
    , (0x4c, astore_1)
    , (0x4d, astore_2)
    , (0x4e, astore_3)
    , (0x4f, iastore)
    , (0x50, lastore)
    , (0x51, fastore)
    , (0x52, dastore)
    , (0x53, aastore)
    , (0x54, bastore)
    , (0x55, castore)
    , (0x56, sastore)
  -- Stack
    , (0x57, pop1)
    , (0x58, pop2)
    , (0x59, dup)
    -- , (0x5a, dup_x1)
    -- , (0x5b, dup_x2)
    -- , (0x5c, dup2)
    -- , (0x5d, dup2_x1)
    -- , (0x5e, dup2_x2)
    -- , (0x5f, swap)
  -- Math
    , (0x60, iadd)
    , (0x61, ladd)
    , (0x62, fadd)
    , (0x63, dadd)
    , (0x64, isub)
    , (0x65, lsub)
    , (0x66, fsub)
    , (0x67, dsub)
    , (0x68, imul)
    , (0x69, lmul)
    , (0x6a, fmul)
    , (0x6b, dmul)
    , (0x6c, idiv)
    , (0x6d, ldiv)
    , (0x6e, fdiv)
    , (0x6f, ddiv)
    , (0x70, irem)
    , (0x71, lrem)
    , (0x72, frem)
    , (0x73, drem)
    , (0x74, ineg)
    , (0x75, lneg)
    , (0x76, fneg)
    , (0x77, dneg)
    , (0x78, ishl)
    , (0x79, lshl)
    , (0x7a, ishr)
    , (0x7b, lshr)
    , (0x7c, iushr)
    , (0x7d, lushr)
    , (0x7e, iand)
    , (0x7f, land)
    , (0x80, ior)
    , (0x81, lor)
    , (0x82, ixor)
    , (0x83, lxor)
    , (0x84, iinc)
  -- -- Conversions
    , (0x85, i2l)
    , (0x86, i2f)
    , (0x87, i2d)
    , (0x88, l2i)
    , (0x89, l2f)
    , (0x8a, l2d)
    , (0x8b, f2i)
    , (0x8c, f2l)
    , (0x8d, f2d)
    , (0x8e, d2i)
    , (0x8f, d2l)
    , (0x90, d2f)
    , (0x91, i2b)
    , (0x92, i2c)
    , (0x93, i2s)
  -- -- Comparisons
    , (0x94, lcmp)
    , (0x95, fcmpl)
    , (0x96, fcmpg)
    , (0x97, dcmpl)
    , (0x98, dcmpg)
    , (0x99, ifeq)
    , (0x9a, ifne)
    , (0x9b, iflt)
    , (0x9c, ifge)
    , (0x9d, ifgt)
    , (0x9e, ifle)
    , (0x9f, if_icmpeq)
    , (0xa0, if_icmpne)
    , (0xa1, if_icmplt)
    , (0xa2, if_icmpge)
    , (0xa3, if_icmpgt)
    , (0xa4, if_icmple)
    , (0xa5, if_acmpeq)
    , (0xa6, if_acmpne)
  -- -- Control
    , (0xa7, goto)
    , (0xa8, jsr)
    , (0xa9, ret)
    , (0xaa, tableswitch)
    , (0xab, lookupswitch)
    , (0xac, ireturn)
    , (0xad, lreturn)
    , (0xae, freturn)
    , (0xaf, dreturn)
    , (0xb0, areturn)
    , (0xb1, _return)
  -- -- References
    , (0xb2, getstatic)
    , (0xb3, putstatic)
    , (0xb4, getfield)
    , (0xb5, putfield)
    , (0xb6, invokevirtual)
    , (0xb7, invokespecial)
    , (0xb8, invokestatic)
    , (0xb9, invokeinterface)
    , (0xba, invokedynamic)
    , (0xbb, new)
    , (0xbc, newarray)
    , (0xbd, anewarray)
    , (0xbe, arraylength)
    , (0xbf, athrow)
    , (0xc0, checkcast)
    , (0xc1, instanceof)
    , (0xc2, monitorenter)
    , (0xc3, monitorexit)
  -- Extended
    , (0xc4, wide)
    , (0xc5, multianewarray)
    , (0xc6, ifnull)
    , (0xc7, ifnonnull)
    , (0xc8, goto_w)
    , (0xc9, jsr_w)
  -- Reserved
    , (0xca, breakpoint)
    , (0xfe, impdep1)
    , (0xff, impdep2)
    ]
