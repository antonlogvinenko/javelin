{-# language ConstraintKinds #-}
module Javelin.Runtime.Instructions where

import           Control.Monad.State.Lazy   (State, runState, state)
import           Data.Bits                  (xor, (.&.), (.|.))
import qualified Data.Map.Lazy              as Map (Map, fromList, lookup, (!))
import           Data.Word                  (Word16, Word32, Word64, Word8)
import           Javelin.Lib.Structures
import           Javelin.Runtime.Thread
import           Javelin.Interpreter.ClassPathLoading (ClassPathLoading, getClassSourcesLayout)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Array.IArray          (array)
import           Debug.Trace
import qualified Javelin.Interpreter.Loading as LI (init)
import           Javelin.Lib.ByteCode.Data      (Instruction(..), CPIndex(..))
import           System.IO                  (writeFile)
import           Flow
import           Javelin.Interpreter.Logging
import           Javelin.Interpreter.Termination
import           Javelin.Interpreter.Loading
import           Javelin.Capability.Classes

--stack exec javelin jvm test.App /Library/Java/JavaVirtualMachines/jdk1.8.0_201.jdk/Contents/Home/jre/lib/rt.jar:main 1
--runJVM "/Library/Java/JavaVirtualMachines/jdk1.8.0_201.jdk/Contents/Home/jre/lib/rt.jar:main" "test.App" []

-- We need to start eexuting commands in main method of main class so we have to make it look like
-- someone called 'invokestatic' on main method of main class: the class is loaded, there is a frame with main class id in it etc

---- 1. enable some sort of logging

-- implement class path layout in terms of tagless final
-- move implementation to modules

type InstructionsMonad m = (ClassPathLoading m, Logging m, Termination m, ClassLoading m, Logging m)

runJVM :: InstructionsMonad m => String -> String -> [String] -> m ()
runJVM classPath mainClass args =
  let main = map (\c -> if c == '.' then '/' else c) mainClass
  in do
    dump "\n" "_______ Starting JVM ________"
    console "Main class arg" mainClass
    console "Main class" main
    cpLayout <- getClassSourcesLayout classPath
    console "Classpath" (_classPath cpLayout)
    dump "classpath.log" cpLayout
    case (Map.!) (_classes cpLayout) main of
      JarFile path -> terminate "Not implemented yet: running JVM from a main class inside jar file"
      ClassFile path -> do
        console "Main class found in class file" path
        let classId = ClassId BootstrapClassLoader main
        mainClassInit <- initClassX classId (newRuntime cpLayout)
        case mainClassInit of
          Left err -> terminate err
          Right rt -> case createMainFrame rt classId of
            Right frame -> runThread 0 $ Thread [frame] rt
            Left err -> terminate err

-- InstructionsMonad plan:
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

runThread :: InstructionsMonad m => Int -> Thread -> m ()
runThread c thread
  | c > 100 = console "Exiting" "abnormally"
  | null $ frames thread = console "Exiting" "normally"
  | otherwise =
      case nextInstructionLine thread of
        Right instruction -> do
          (thread2, execution) <- execute instruction thread
          let (_, thread3) = runState execution thread2
          runThread (c + 1) (incrementInstructionCounter thread3)
        Left error -> terminate error

incrementInstructionCounter :: Thread -> Thread
incrementInstructionCounter t@Thread{frames=(f@Frame{pc=pc}:fs)} = t{frames=f{pc=pc+1}:fs}

nextInstructionLine :: Thread -> Either VMError Instruction
nextInstructionLine Thread{frames=(Frame{pc=pc,
                                         currentClass=classId,
                                         currentMethod=methodIndex}):_,
                           runtime=rt} = do
  method <- getMethodByIndex rt classId methodIndex
  return $ instructions method !! pc

pureInstruction :: InstructionsMonad m => (ThreadOperation ()) -> Thread -> m (Thread, ThreadOperation ())
pureInstruction threadOperation thread = return (thread, threadOperation)

impureInstruction :: InstructionsMonad m => (ThreadOperation ()) -> (Thread -> m Thread) -> Thread -> m (Thread, ThreadOperation ())
impureInstruction threadOperation threadModification thread = do
  thread2 <- threadModification thread
  return (thread2, threadOperation)

execute :: InstructionsMonad m => Instruction -> Thread -> m (Thread, ThreadOperation ())
execute Nop = pureInstruction empty

-- resolve field -> resolve class -> load class
-- init class
-- add VMError and IO to ThreadIntruction
execute (GetStatic (CPIndex index)) = \t ->
  do
    --loadedClass <- getClass rt classId
    --let pool = symTable loadedClass
    --fieldReference = getStringLiteral pool index
    console "doing" "getstatic"
    return (t, empty)

-- push const <i> to stack
execute IConstM1 = pureInstruction $ iconst (-1)
execute IConst0 = pureInstruction $ iconst 0
execute IConst1 = pureInstruction $ iconst 1
execute IConst2 = pureInstruction $ iconst 2
execute IConst3 = pureInstruction $ iconst 3
execute IConst4 = pureInstruction $ iconst 4
execute IConst5 = pureInstruction $ iconst 5

execute LConst0 = pureInstruction $ lconst 0
execute LConst1 = pureInstruction $ lconst 1

execute FConst0 = pureInstruction $ fconst 0
execute FConst1 = pureInstruction $ fconst 1
execute FConst2 = pureInstruction $ fconst 2

execute DConst0 = pureInstruction $ dconst 0
execute DConst1 = pureInstruction $ dconst 1

-- pop int from stack, put to <i> local variable
execute IStore0 = pureInstruction $ popAndStoreAt jint 0
execute IStore1 = pureInstruction $ popAndStoreAt jint 1
execute IStore2 = pureInstruction $ popAndStoreAt jint 2
execute IStore3 = pureInstruction $ popAndStoreAt jint 3
execute (IStore localId) = pureInstruction $ popAndStoreAt jint localId

execute LStore0 = pureInstruction $ popAndStoreAt jlong 0
execute LStore1 = pureInstruction $ popAndStoreAt jlong 1
execute LStore2 = pureInstruction $ popAndStoreAt jlong 2
execute LStore3 = pureInstruction $ popAndStoreAt jlong 3
execute (LStore localId) = pureInstruction $ popAndStoreAt jint localId

execute FStore0 = pureInstruction $ popAndStoreAt jfloat 0
execute FStore1 = pureInstruction $ popAndStoreAt jfloat 1
execute FStore2 = pureInstruction $ popAndStoreAt jfloat 2
execute FStore3 = pureInstruction $ popAndStoreAt jfloat 3
execute (FStore localId) = pureInstruction $ popAndStoreAt jfloat localId

execute DStore0 = pureInstruction $ popAndStoreAt jdouble 0
execute DStore1 = pureInstruction $ popAndStoreAt jdouble 1
execute DStore2 = pureInstruction $ popAndStoreAt jdouble 2
execute DStore3 = pureInstruction $ popAndStoreAt jdouble 3
execute (DStore localId) = pureInstruction $ popAndStoreAt jdouble localId

-- read int from local var <i>, push it to stack
execute (ILoad local)  = pureInstruction $ loadAndPushAt jint local
execute ILoad0 = pureInstruction $ loadAndPushAt jint 0

execute ILoad1 = pureInstruction $ loadAndPushAt jint 1
execute ILoad2 = pureInstruction $ loadAndPushAt jint 2
execute ILoad3 = pureInstruction $ loadAndPushAt jint 3

execute (LLoad local) = pureInstruction $ loadAndPushAt jlong local
execute LLoad0 = pureInstruction $ loadAndPushAt jlong 0
execute LLoad1 = pureInstruction $ loadAndPushAt jlong 1
execute LLoad2 = pureInstruction $ loadAndPushAt jlong 2
execute LLoad3 = pureInstruction $ loadAndPushAt jlong 3

execute (FLoad local) = pureInstruction $ loadAndPushAt jfloat local
execute FLoad0 = pureInstruction $ loadAndPushAt jfloat 0
execute FLoad1 = pureInstruction $ loadAndPushAt jfloat 1
execute FLoad2 = pureInstruction $ loadAndPushAt jfloat 2
execute FLoad3 = pureInstruction $ loadAndPushAt jfloat 3

execute (DLoad local) = pureInstruction $ loadAndPushAt jdouble local
execute DLoad0 = pureInstruction $ loadAndPushAt jdouble 0
execute DLoad1 = pureInstruction $ loadAndPushAt jdouble 1
execute DLoad2 = pureInstruction $ loadAndPushAt jdouble 2
execute DLoad3 = pureInstruction $ loadAndPushAt jdouble 3

execute IAdd = pureInstruction $ add jint
execute LAdd = pureInstruction $ add jlong
execute FAdd = pureInstruction $ add jfloat
execute DAdd = pureInstruction $ add jdouble

execute Return = pureInstruction $ dropTopFrame
  
-- execute (InvokeStatic (CPIndex index)) = undefined

-- Instructions implementation
-- Constants
nop = empty

iconst x = push (x :: JInt)

lconst x = push (x :: JLong)

fconst x = push (x :: JFloat)

dconst x = push (x :: JDouble)

aconst_null = iconst 0

bipush = do
  byte <- arg jbyte 0
  push $ signExtend byte

sipush = do
  short <- arg jshort 0
  push $ signExtend short

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

neg operandType = do
  x <- pop operandType
  push $ -x

ineg = neg jint

lneg = neg jlong

fneg = neg jfloat

dneg = neg jdouble

iand = math jint (.&.)

land = math jlong (.&.)

ior = math jint (.|.)

lor = math jlong (.|.)

ixor = math jint xor

lxor = math jlong xor

-- wrong implementation: bytes are read from instruction not local arguments
iinc = do
  index <- arg jLocalRef 0
  const <- arg jbyte 0
  var <- load jint index
  let constExt = signExtend const
  let newVar = var + constExt
  store newVar index

