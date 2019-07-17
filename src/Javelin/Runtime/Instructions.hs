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
import           Javelin.ByteCode.Data      (Instruction(..), CPIndex(..))
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

execute LConst0 = lconst 0
execute LConst1 = lconst 1

execute FConst0 = fconst 0
execute FConst1 = fconst 1
execute FConst2 = fconst 2

execute DConst0 = dconst 0
execute DConst1 = dconst 1

-- pop int from stack, put to <i> local variable
execute IStore0 = popAndStoreAt jint 0
execute IStore1 = popAndStoreAt jint 1
execute IStore2 = popAndStoreAt jint 2
execute IStore3 = popAndStoreAt jint 3
execute (IStore localId) = popAndStoreAt jint localId

execute LStore0 = popAndStoreAt jlong 0
execute LStore1 = popAndStoreAt jlong 1
execute LStore2 = popAndStoreAt jlong 2
execute LStore3 = popAndStoreAt jlong 3
execute (LStore localId) = popAndStoreAt jint localId

execute FStore0 = popAndStoreAt jfloat 0
execute FStore1 = popAndStoreAt jfloat 1
execute FStore2 = popAndStoreAt jfloat 2
execute FStore3 = popAndStoreAt jfloat 3
execute (FStore localId) = popAndStoreAt jfloat localId

execute DStore0 = popAndStoreAt jdouble 0
execute DStore1 = popAndStoreAt jdouble 1
execute DStore2 = popAndStoreAt jdouble 2
execute DStore3 = popAndStoreAt jdouble 3
execute (DStore localId) = popAndStoreAt jdouble localId

-- read int from local var <i>, push it to stack
execute (ILoad local)  = loadAndPushAt jint local
execute ILoad0 = loadAndPushAt jint 0
execute ILoad1 = loadAndPushAt jint 1
execute ILoad2 = loadAndPushAt jint 2
execute ILoad3 = loadAndPushAt jint 3

execute (LLoad local) = loadAndPushAt jlong local
execute LLoad0 = loadAndPushAt jlong 0
execute LLoad1 = loadAndPushAt jlong 1
execute LLoad2 = loadAndPushAt jlong 2
execute LLoad3 = loadAndPushAt jlong 3

execute (FLoad local) = loadAndPushAt jfloat local
execute FLoad0 = loadAndPushAt jfloat 0
execute FLoad1 = loadAndPushAt jfloat 1
execute FLoad2 = loadAndPushAt jfloat 2
execute FLoad3 = loadAndPushAt jfloat 3

execute (DLoad local) = loadAndPushAt jdouble local
execute DLoad0 = loadAndPushAt jdouble 0
execute DLoad1 = loadAndPushAt jdouble 1
execute DLoad2 = loadAndPushAt jdouble 2
execute DLoad3 = loadAndPushAt jdouble 3

execute IAdd = add jint
execute LAdd = add jlong
execute FAdd = add jfloat
execute DAdd = add jdouble

execute Return = dropTopFrame
-- resolve field -> resolve class -> load class
-- init class
-- add VMError and IO to ThreadIntruction
execute (GetStatic (CPIndex index)) = state $ \t@Thread{runtime=rt,
                                              frames=frames@(Frame{currentClass=classId}:_)} ->
  let fieldReference = do
        loadedClass <- getClass rt classId
        let pool = symTable loadedClass
        getStringLiteral pool index
  in do
    rt <- undefined --resolve field
    rt <- undefined --init class
    return undefined
execute (InvokeStatic (CPIndex index)) = undefined

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

