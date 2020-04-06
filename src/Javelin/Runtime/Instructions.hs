module Javelin.Runtime.Instructions where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (runStateT)
import Data.Array.IArray (array)
import Data.Bits (shiftR, shiftL, (.&.), (.|.), xor)
import qualified Data.Map.Lazy as Map ((!))
import Javelin.Capability.Classes
import Javelin.Lib.ByteCode.Data (CPIndex(..), Instruction(..))
import Javelin.Lib.Structures
import Javelin.Runtime.Thread

-- runJVM "./sample-classpath/rt.jar:main" "test.App" []
-- cabal run --verbose=0 javelin jvm javelin.SumOfIntegers sample-classpath/rt.jar:test-programs-output 1
runJVM :: Global m => String -> String -> [String] -> m ()
runJVM classPath mainClass args =
  let main =
        map
          (\c ->
             if c == '.'
               then '/'
               else c)
          mainClass
   in do dump "\n" "_______ Starting JVM ________"
         console "Main class arg" mainClass
         console "Main class" main
         cpLayout <- getClassSourcesLayout classPath
         console "Classpath" (_classPath cpLayout)
         dump "classpath.log" cpLayout
         case (Map.!) (_classes cpLayout) main of
           JarFile path ->
             terminate
               "Not implemented yet: running JVM from a main class inside jar file"
           ClassFile path -> do
             console "Main class found in class file" path
             let classId = ClassId BootstrapClassLoader main
             mainClassInit <- initClass classId (newRuntime cpLayout)
             case mainClassInit of
               Left err -> terminate err
               Right rt ->
                 case createMainFrame rt classId of
                   Right frame -> runThread 0 $ Thread [frame] rt
                   Left err -> terminate err

-- Global plan:
-- 1. create a small snippet
-- 2. do everything for a single snippet: make it work
-- 3. write an acceptance test for the working snippet
-- 4. write unit test for new code
-- 6. do all similar functionality (similar commands)
-- 7. new acceptance & unit tests for 6
-- 8. required updates of testing infrastructure
-- goto 1: next snippet
-- todo:
-- replace string with text
-- remove missingH
-- replace Prelude with smth else
-- lazy/strict state
-- doc: project structure
-- reader tutorial
-- todo finish passing arguments -- but first finish 'currentFrame'
-- more general acceptance tests: build java main classes when executing
createMainFrame :: Runtime -> ClassId -> Either VMError Frame
createMainFrame rt classId =
  createFrame rt classId (PartReference "main" "(Ljava/lang/String[];)V")

-- todo finish: stack depth, local args size, arguments size
createFrame :: Runtime -> ClassId -> PartReference -> Either VMError Frame
createFrame rt classId methodReference =
  case getMethodBySignature rt classId methodReference of
    Right (index, method) ->
      let localsLength = fromIntegral $ localsSize method
          initValue = [(i, 0) | i <- [0 .. localsLength - 1]]
          locals = Locals $ array (0, localsLength - 1) initValue
       in Right $ Frame 0 classId index locals []
    Left err -> Left err

getTopFrame :: Thread -> Maybe Frame
getTopFrame t =
  let allFrames = frames t
   in case allFrames of
        [] -> Nothing
        (f:fs) -> Just f

runThread :: Global m => Int -> Thread -> m ()
runThread c thread
  | c > 100 = console "Exiting" "abnormally"
  | null $ frames thread = console "Exiting" "normally"
  | otherwise =
    case nextInstructionLine thread of
      Right instruction -> do
        console "Instruction:" instruction
        let thread1 = incrementInstructionCounter thread
        (thread2, execution) <- execute instruction thread1
        (_, thread3) <- runIO $! runStateT execution thread2
        let topFrame = getTopFrame thread3
        console " stack:" $ operands <$> topFrame
        console " local:" $ locals <$> topFrame
        say ""
        runThread (c + 1) thread3
      Left error -> terminate error

incrementInstructionCounter :: Thread -> Thread
incrementInstructionCounter t@Thread {frames = (f@Frame {pc = pc}:fs)} =
  t {frames = f {pc = pc + 1} : fs}

nextInstructionLine :: Thread -> Either VMError Instruction
nextInstructionLine Thread { frames = Frame { pc = pc
                                            , currentClass = classId
                                            , currentMethod = methodIndex
                                            }:_
                           , runtime = rt
                           } = do
  method <- getMethodByIndex rt classId methodIndex
  return $ instructions method !! pc

pureInstruction ::
     Global m => ThreadOperation () -> Thread -> m (Thread, ThreadOperation ())
pureInstruction threadOperation thread = return (thread, threadOperation)

at :: Integral b => [a] -> b -> a
at cc i = cc !! (fromIntegral i - 1)

impureInstruction ::
     Global m
  => ThreadOperation ()
  -> (Thread -> m Thread)
  -> Thread
  -> m (Thread, ThreadOperation ())
impureInstruction threadOperation threadModification thread = do
  thread2 <- threadModification thread
  return (thread2, threadOperation)

out =
  FieldReference $
  ClassPartReference
    (PartReference "java/lang/System" "out")
    "Ljava/io/PrintStream;"


-- shift :: (JType j) => (j -> Int -> j) -> (Int -> StackElement -> j) -> ThreadOperation ()
shift operation operandType = do
  op2 <- pop jint
  op1 <- pop operandType
  push $ operation op1 (fromIntegral op2)

uShiftR :: (JType j, Integral j) => j -> Int -> j
uShiftR n k = fromIntegral (fromIntegral n `shiftR` k :: Word)

outReference :: JReference
outReference = maxBound

execute :: Global m => Instruction -> Thread -> m (Thread, ThreadOperation ())
execute Nop = pureInstruction empty
execute BreakPoint = pureInstruction empty
execute ImDep1 = pureInstruction empty
execute ImDep2 = pureInstruction empty
-- resolve field -> resolve class -> load class
execute v@(GetStatic (CPIndex index)) =
  \t@Thread { frames = Frame { pc = pc
                             , currentClass = classId
                             , currentMethod = methodIndex
                             }:_
            , runtime = rt
            } ->
    let eitherSymTable = symTable <$> getClass rt classId
     in case eitherSymTable of
          Left err -> undefined
          Right symTable
            -- console "index" index
            -- console "symTable" symTable
           -> do
            let classFieldReference = symTable `at` index
            if classFieldReference == out
              then return (t, push outReference)
                -- then do field resolving and class init
              else return (t, empty)
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
execute FConst0 = pureInstruction $ fconst 0.0
execute FConst1 = pureInstruction $ fconst 1.0
execute FConst2 = pureInstruction $ fconst 2.0
execute DConst0 = pureInstruction $ dconst 0.0
execute DConst1 = pureInstruction $ dconst 1.0
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
execute (LStore localId) = pureInstruction $ popAndStoreAt jlong localId
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
execute (ILoad local) = pureInstruction $ loadAndPushAt jint local
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
execute IAdd = pureInstruction $ math (+) jint
execute LAdd = pureInstruction $ math (+) jlong
execute FAdd = pureInstruction $ math (+) jfloat
execute DAdd = pureInstruction $ math (+) jdouble
execute IMul = pureInstruction $ math (*) jint
execute LMul = pureInstruction $ math (*) jlong
execute FMul = pureInstruction $ math (*) jfloat
execute DMul = pureInstruction $ math (*) jdouble
execute ISub = pureInstruction $ math (-) jint
execute LSub = pureInstruction $ math (-) jlong
execute FSub = pureInstruction $ math (-) jfloat
execute DSub = pureInstruction $ math (-) jdouble
execute INeg = pureInstruction $ neg jint
execute LNeg = pureInstruction $ neg jlong
execute FNeg = pureInstruction $ neg jfloat
execute DNeg = pureInstruction $ neg jdouble
execute IOr = pureInstruction $ math (.|.) jint
execute IAnd = pureInstruction $ math (.&.) jint
execute IXor = pureInstruction $ math xor jint
execute IShl = pureInstruction $ shift shiftL jint
execute IShr = pureInstruction $ shift shiftR jint
execute IUshr = pureInstruction $ shift uShiftR jint
execute LOr = pureInstruction $ math (.|.) jlong
execute LAnd = pureInstruction $ math (.&.) jlong
execute LXor = pureInstruction $ math xor jlong
execute LShl = pureInstruction $ shift shiftL jlong
execute LShr = pureInstruction $ shift shiftR jlong
execute LUshr = pureInstruction $ shift uShiftR jlong

execute Return = pureInstruction dropTopFrame
execute (InvokeVirtual (CPIndex index)) =
  \t@Thread { frames = (frame@Frame { pc = pc
                                    , currentClass = classId
                                    , currentMethod = methodIndex
                                    }):_
            , runtime = rt
            } -> do
    console "operands frame" (operands frame !! 1)
    let eitherSymTable = symTable <$> getClass rt classId
    case eitherSymTable of
      Left err -> undefined
      Right symTable -> do
        let methodReference = symTable `at` index
        console "method reference" methodReference
        let owner = _ownerName (classMethod methodReference)
        let stringValue = typeFormatter owner
        let classFieldReference = symTable `at` index
        return
          ( t
          , do value <- stringValue
               object <- pop jreference
               if outReference == object
                 then liftIO $ putStr value
                 else empty)

typeFormatter :: String -> ThreadOperation String
typeFormatter x
  | x == "(F)V" = show <$> pop jfloat
  | x == "(I)V" = show <$> pop jint
  | x == "(J)V" = show <$> pop jlong
  | x == "(D)V" = show <$> pop jdouble

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
pop1 = remove

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
math operation operandType = do
  op2 <- pop operandType
  op1 <- pop operandType
  push $ operation op1 op2

idiv = math div jint

ldiv = math div jlong

fdiv = math (/) jfloat

ddiv = math (/) jdouble

irem = math rem jint

lrem = math rem jlong

neg operandType = do
  x <- pop operandType
  push $ -x

-- wrong implementation: bytes are read from instruction not local arguments
iinc = do
  index <- arg jLocalRef 0
  const <- arg jbyte 0
  var <- load jint index
  let constExt = signExtend const
  let newVar = var + constExt
  store newVar index
