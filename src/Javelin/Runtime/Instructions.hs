module Javelin.Runtime.Instructions where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.State.Lazy as State
import qualified Data.Array.IArray as Array
import qualified Data.Bits as Bits
import qualified Data.Map.Lazy as Map
import qualified Data.Word as Word

import Javelin.Capability.Classes
import Javelin.Lib.ByteCode.Data (CPIndex(..), Instruction(..))
import Javelin.Lib.Structures
import Javelin.Runtime.Thread

-- createJVM "./sample-classpath/rt.jar:main" "test.App" []
-- cabal run --verbose=0 javelin jvm javelin.SumOfIntegers sample-classpath/rt.jar:test-programs-output 1
createJVM :: Global m => String -> String -> [String] -> m ()
createJVM classPath mainClass args =
  let main =
        map
          (\c ->
             if c == '.'
               then '/'
               else c)
          mainClass
   in do dump "\n" "_______ Starting JVM ________"
         console "Main class" main
         cpLayout <- getClassSourcesLayout classPath
         console "Reading classpath" (_classPath cpLayout)
         dump "classpath.log" cpLayout
         case (Map.!) (_classes cpLayout) main of
           JarFile path ->
             terminate
               "Not implemented yet: running JVM from a main class inside jar file"
           ClassFile path -> do
             console "Main class found in class file" path
             let classId = ClassId BootstrapClassLoader main
             mainClassInit <- initClass classId (newRuntime cpLayout)
             msg "Main class loaded"
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
          locals = Locals $ Array.array (0, localsLength - 1) initValue
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
        (_, thread3) <- runIO $! State.runStateT execution thread2
        let topFrame = getTopFrame thread3
        console " stack:" $ operands <$> topFrame
        console " local:" $ locals <$> topFrame
        emptyLine
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

doNothing :: (Global m) => Thread -> m (Thread, ThreadOperation ())
doNothing t = return (t, empty)

-- shift :: (JType j) => (j -> Int -> j) -> (Int -> StackElement -> j) -> ThreadOperation ()
shift operation operandType = do
  op2 <- pop jint
  op1 <- pop operandType
  push $ operation op1 (fromIntegral op2)

ulShiftR :: JLong -> Int -> JLong
ulShiftR n k = fromIntegral $ Bits.shiftR (fromIntegral n :: Word.Word64) k

uiShiftR :: JInt -> Int -> JInt
uiShiftR n k = fromIntegral $ Bits.shiftR (fromIntegral n :: Word.Word32) k

outReference :: JReference
outReference = maxBound

undefinedInstruction :: Global m => Thread -> m (Thread, ThreadOperation ())
undefinedInstruction = pureInstruction empty

execute :: Global m => Instruction -> Thread -> m (Thread, ThreadOperation ())

-- push const <i> to stack
execute Nop = undefinedInstruction
execute AConstNull = pureInstruction $ aconst_null
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
execute (BiPush value) = pureInstruction $ push (fromIntegral value :: JByte)
execute (SiPush value) = pureInstruction $ push (fromIntegral value :: JShort)
execute (Ldc (CPIndex idx)) = \t@Thread {frames = frame@Frame { pc = pc, currentClass = classId, currentMethod = methodIndex}:_, runtime = rt} ->
  do
    let eitherSymTable = symTable <$> getClass rt classId
    case eitherSymTable of
      Left err -> terminate "Failed to access constant pool" >> doNothing t
      Right symTable -> do
        let valueReference = symTable `at` idx :: SymbolicReference
        case valueReference of
          IntegerLiteral value -> return (t, push value)
          FloatLiteral value -> return (t, push value)
          _ -> terminate "not implemented yet" >> doNothing t
execute (LdcW (CPIndex idx)) = undefinedInstruction
execute (Ldc2W (CPIndex idx)) = \t@Thread {frames = frame@Frame { pc = pc, currentClass = classId, currentMethod = methodIndex}:_, runtime = rt} ->
  do
    let eitherSymTable = symTable <$> getClass rt classId
    case eitherSymTable of
      Left err -> terminate "Failed to access constant pool" >> doNothing t
      Right symTable -> do
        let valueReference = symTable `at` idx :: SymbolicReference
        case valueReference of
          LongLiteral value -> return (t, push value)
          DoubleLiteral value -> return (t, push value)
          _ -> terminate "Long value expected in constant pool" >> doNothing t

-- Loads, read int from local var <i>, push it to stack
execute (ILoad local) = pureInstruction $ loadAndPushAt jint local
execute (LLoad local) = pureInstruction $ loadAndPushAt jlong local
execute (FLoad local) = pureInstruction $ loadAndPushAt jfloat local
execute (DLoad local) = pureInstruction $ loadAndPushAt jdouble local
execute ILoad0 = pureInstruction $ loadAndPushAt jint 0
execute ILoad1 = pureInstruction $ loadAndPushAt jint 1
execute ILoad2 = pureInstruction $ loadAndPushAt jint 2
execute ILoad3 = pureInstruction $ loadAndPushAt jint 3
execute LLoad0 = pureInstruction $ loadAndPushAt jlong 0
execute LLoad1 = pureInstruction $ loadAndPushAt jlong 1
execute LLoad2 = pureInstruction $ loadAndPushAt jlong 2
execute LLoad3 = pureInstruction $ loadAndPushAt jlong 3
execute FLoad0 = pureInstruction $ loadAndPushAt jfloat 0
execute FLoad1 = pureInstruction $ loadAndPushAt jfloat 1
execute FLoad2 = pureInstruction $ loadAndPushAt jfloat 2
execute FLoad3 = pureInstruction $ loadAndPushAt jfloat 3
execute DLoad0 = pureInstruction $ loadAndPushAt jdouble 0
execute DLoad1 = pureInstruction $ loadAndPushAt jdouble 1
execute DLoad2 = pureInstruction $ loadAndPushAt jdouble 2
execute DLoad3 = pureInstruction $ loadAndPushAt jdouble 3
execute ALoad0 = pureInstruction $ loadAndPushAt jreference 0
execute ALoad1 = pureInstruction $ loadAndPushAt jreference 1
execute ALoad2 = pureInstruction $ loadAndPushAt jreference 2
execute ALoad3 = pureInstruction $ loadAndPushAt jreference 3
execute IaLoad = undefinedInstruction
execute LaLoad = undefinedInstruction
execute FaLoad = undefinedInstruction
execute DaLoad = undefinedInstruction
execute AaLoad = undefinedInstruction
execute BaLoad = undefinedInstruction
execute CaLoad = undefinedInstruction
execute SaLoad = undefinedInstruction

-- Stores, pop int from stack, put to <i> local variable
execute (IStore localId) = pureInstruction $ popAndStoreAt jint localId
execute (LStore localId) = pureInstruction $ popAndStoreAt jlong localId
execute (FStore localId) = pureInstruction $ popAndStoreAt jfloat localId
execute (DStore localId) = pureInstruction $ popAndStoreAt jdouble localId
execute (AStore localId) = pureInstruction $ popAndStoreAt jreference localId
execute IStore0 = pureInstruction $ popAndStoreAt jint 0
execute IStore1 = pureInstruction $ popAndStoreAt jint 1
execute IStore2 = pureInstruction $ popAndStoreAt jint 2
execute IStore3 = pureInstruction $ popAndStoreAt jint 3
execute LStore0 = pureInstruction $ popAndStoreAt jlong 0
execute LStore1 = pureInstruction $ popAndStoreAt jlong 1
execute LStore2 = pureInstruction $ popAndStoreAt jlong 2
execute LStore3 = pureInstruction $ popAndStoreAt jlong 3
execute FStore0 = pureInstruction $ popAndStoreAt jfloat 0
execute FStore1 = pureInstruction $ popAndStoreAt jfloat 1
execute FStore2 = pureInstruction $ popAndStoreAt jfloat 2
execute FStore3 = pureInstruction $ popAndStoreAt jfloat 3
execute DStore0 = pureInstruction $ popAndStoreAt jdouble 0
execute DStore1 = pureInstruction $ popAndStoreAt jdouble 1
execute DStore2 = pureInstruction $ popAndStoreAt jdouble 2
execute DStore3 = pureInstruction $ popAndStoreAt jdouble 3
execute AStore0 = pureInstruction $ popAndStoreAt jreference 0
execute AStore1 = pureInstruction $ popAndStoreAt jreference 0
execute AStore2 = pureInstruction $ popAndStoreAt jreference 0
execute AStore3 = pureInstruction $ popAndStoreAt jreference 0
execute IaStore = undefinedInstruction
execute LaStore = undefinedInstruction
execute FaStore = undefinedInstruction
execute DaStore = undefinedInstruction
execute AaStore = undefinedInstruction
execute BaStore = undefinedInstruction
execute CaStore = undefinedInstruction
execute SaStore = undefinedInstruction
-- Stack
execute Pop = pureInstruction $ pop jint >> empty
execute Pop2 = pureInstruction $ popn jint 2 >> empty
execute Dup = undefinedInstruction
execute DupX1 = undefinedInstruction
execute DupX2 = undefinedInstruction
execute Dup2 = undefinedInstruction
execute Dup2X1 = undefinedInstruction
execute Dup2X2 = undefinedInstruction
execute Swap = undefinedInstruction
-- Math
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
execute IOr = pureInstruction $ math (Bits..|.) jint
execute IAnd = pureInstruction $ math (Bits..&.) jint
execute IXor = pureInstruction $ math Bits.xor jint
execute IShl = pureInstruction $ shift Bits.shiftL jint
execute IShr = pureInstruction $ shift Bits.shiftR jint
execute IUshr = pureInstruction $ shift uiShiftR jint
execute LOr = pureInstruction $ math (Bits..|.) jlong
execute LAnd = pureInstruction $ math (Bits..&.) jlong
execute LXor = pureInstruction $ math Bits.xor jlong
execute LShl = pureInstruction $ shift Bits.shiftL jlong
execute LShr = pureInstruction $ shift Bits.shiftR jlong
execute LUshr = pureInstruction $ shift ulShiftR jlong
execute IDiv = pureInstruction $ math div jint
execute LDiv = pureInstruction $ math div jlong
execute FDiv = pureInstruction $ math (/) jfloat
execute DDiv = pureInstruction $ math (/) jdouble
execute IRem = pureInstruction $ math rem jint
execute LRem = pureInstruction $ math rem jlong
execute FRem = undefinedInstruction
execute DRem = undefinedInstruction
execute (IInc localId word8) = undefinedInstruction

-- Conversions
execute I2L = undefinedInstruction
execute I2F = undefinedInstruction
execute I2D = undefinedInstruction
execute L2I = undefinedInstruction
execute L2F = undefinedInstruction
execute L2D = undefinedInstruction
execute F2I = undefinedInstruction
execute F2L = undefinedInstruction
execute F2D = undefinedInstruction
execute D2I = undefinedInstruction
execute D2L = undefinedInstruction
execute D2F = undefinedInstruction
execute I2B = undefinedInstruction
execute I2C = undefinedInstruction
execute I2S = undefinedInstruction
  
-- Comparisons
execute LCmp = undefinedInstruction
execute FCmpL = undefinedInstruction
execute FCmpG = undefinedInstruction
execute DCmpL = undefinedInstruction
execute DCmpG = undefinedInstruction
execute (IfEq branchOffset) = undefinedInstruction
execute (IfNe branchOffset) = undefinedInstruction
execute (IfLt branchOffset) = undefinedInstruction
execute (IfGe branchOffset) = undefinedInstruction
execute (IfGt branchOffset) = undefinedInstruction
execute (IfLe branchOffset) = undefinedInstruction
execute (IfICmpEq branchOffset) = undefinedInstruction
execute (IfICmpNe branchOffset) = undefinedInstruction
execute (IfICmpLt branchOffset) = undefinedInstruction
execute (IfICmpGe branchOffset) = undefinedInstruction
execute (IfICmpGt branchOffset) = undefinedInstruction
execute (IfICmpLe branchOffset) = undefinedInstruction
execute (IfACmpEq branchOffset) = undefinedInstruction
execute (IfACmpNe branchOffset) = undefinedInstruction

-- Control
execute (Goto branchOffset) = undefinedInstruction
execute (Jsr branchOffset) = undefinedInstruction
execute (Ret localId) = undefinedInstruction
execute (TableSwitch a b c d) = undefinedInstruction
execute (LookupSwitch a b) = undefinedInstruction
execute IReturn = undefinedInstruction
execute LReturn = undefinedInstruction
execute FReturn = undefinedInstruction
execute DReturn = undefinedInstruction
execute AReturn = undefinedInstruction
execute Return = pureInstruction dropTopFrame

-- References
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
execute (PutStatic cpIndex) = undefinedInstruction
execute (GetField cpIndex) = undefinedInstruction
execute (PutField cpIndex) = undefinedInstruction

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
                 then MonadIO.liftIO $ putStr value
                 else empty)
execute (InvokeSpecial cpIndex) = undefinedInstruction
execute (InvokeStatic cpIndex) = undefinedInstruction
execute (InvokeInterface cpIndex idx) = undefinedInstruction
execute (InvokeDynamic cpIndex idx) = undefinedInstruction
execute (New_ cpIndex) = undefinedInstruction
execute (NewArray word8) = undefinedInstruction
execute (ANewArray cpIndex) = undefinedInstruction
execute ArrayLength = undefinedInstruction
execute AThrow = undefinedInstruction
execute (CheckCast cpIndex) = undefinedInstruction
execute (InstanceOf_ cpIndex) = undefinedInstruction
execute MonitorEnter = undefinedInstruction
execute MonitorExit = undefinedInstruction

-- Extended
execute (WideIInc cpIndex word16) = undefinedInstruction
execute (WideILoad cpIndex) = undefinedInstruction
execute (WideFLoad cpIndex) = undefinedInstruction
execute (WideALoad cpIndex) = undefinedInstruction
execute (WideLLoad cpIndex) = undefinedInstruction
execute (WideDLoad cpIndex) = undefinedInstruction
execute (WideIStore cpIndex) = undefinedInstruction
execute (WideFStore cpIndex) = undefinedInstruction
execute (WideAStore cpIndex) = undefinedInstruction
execute (WideLStore cpIndex) = undefinedInstruction
execute (WideDStore cpIndex) = undefinedInstruction
execute (WideRet cpIndex) = undefinedInstruction
execute (MultiANewArray cpIndex word8) = undefinedInstruction
execute (IfNull cpIndex) = undefinedInstruction
execute (IfNotNull cpIndex) = undefinedInstruction
execute (GotoW word32) = undefinedInstruction
execute (JsrW word32) = undefinedInstruction

-- Reserved
execute BreakPoint = undefinedInstruction
execute ImDep1 = undefinedInstruction
execute ImDep2 = undefinedInstruction


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
