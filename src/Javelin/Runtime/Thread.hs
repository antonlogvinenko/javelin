module Javelin.Runtime.Thread where

import Control.Monad.State.Lazy (StateT, state)
import Data.Array.IArray (Array, (!), (//))
import Data.Binary.Get (getWord64be, runGet)
import Data.Binary.IEEE754
  ( doubleToWord
  , floatToWord
  , wordToDouble
  , wordToFloat
  )
import Data.Binary.Put
import Data.Bits (rotate)
import Data.ByteString.Lazy (pack)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Lazy as Map (Map, fromList)
import Data.Word (Word16, Word32, Word64, Word8)

import Javelin.Lib.Structures

step :: Thread -> Thread
step e = e

nativeJVM :: Map.Map (ClassName, String) (Thread -> Thread)
nativeJVM = Map.fromList [(("java.lang.Class", "getClass"), id)]

-- Bytes storage order
class BytesContainer c where
  getBytes :: c -> Int -> Int -> Word64

newtype Arguments =
  Arguments
    { argumentsArray :: [Word8]
    }
  deriving (Show, Eq)

instance BytesContainer Arguments where
  getBytes c idx len = argumentToWord64 $ take len $ drop idx $ argumentsArray c

argumentToWord64 :: [Word8] -> Word64
argumentToWord64 bs =
  let normalized = take (8 - length bs) bs ++ bs
   in runGet getWord64be $ pack normalized

instance BytesContainer Locals where
  getBytes c idx len =
    let arr = vars c
     in if len <= 4
          then localToWord64 [0, arr ! idx]
          else localToWord64 [arr ! idx, arr ! (idx + 1)]

localToWord64 :: [Word32] -> Word64
localToWord64 bs =
  runGet getWord64be $ runPut $ putWord32be (head bs) >> putWord32be (bs !! 1)

instance BytesContainer StackElement where
  getBytes c idx len = stackElement c

-- Primitive types
data Representation
  = Narrow
      { half :: Word32
      }
  | Wide
      { full :: Word64
      }

type JLocalRef = Word8

type JByte = Int8

type JShort = Int16

type JInt = Int32

type JLong = Int64

type JBoolean = JInt

type JChar = Word16

type JDouble = Double

type JFloat = Float

type JRaw = Word64

type JReference = Word64

narrowInt :: (Integral a) => a -> Representation
narrowInt = Narrow . fromIntegral

wideInt :: (Integral a) => a -> Representation
wideInt = Wide . fromIntegral

class Show a =>
      JType a
  where
  represent :: a -> Representation

instance JType Word8 where
  represent = narrowInt

instance JType Int8 where
  represent = narrowInt

instance JType Int16 where
  represent = narrowInt

instance JType Int32 where
  represent = narrowInt

instance JType Int64 where
  represent = wideInt

instance JType Word16 where
  represent = narrowInt

instance JType Double where
  represent = Wide . doubleToWord

instance JType Float where
  represent = Narrow . floatToWord

instance JType Word64 where
  represent = Wide

fetchBytes :: (BytesContainer c, Num a) => Int -> Int -> c -> a
fetchBytes len idx c = fromIntegral $ getBytes c idx len

jraw :: (BytesContainer c) => Int -> c -> JRaw
jraw = fetchBytes 8

jbyte :: (BytesContainer c) => Int -> c -> JByte
jbyte = fetchBytes 1

jLocalRef :: (BytesContainer c) => Int -> c -> JLocalRef
jLocalRef = fetchBytes 1

jshort :: (BytesContainer c) => Int -> c -> JShort
jshort = fetchBytes 2

jint :: (BytesContainer c) => Int -> c -> JInt
jint = fetchBytes 4

jlong :: (BytesContainer c) => Int -> c -> JLong
jlong = fetchBytes 8

jchar :: (BytesContainer c) => Int -> c -> JChar
jchar = fetchBytes 2

jboolean :: (BytesContainer c) => Int -> c -> JBoolean
jboolean = fetchBytes 1

jfloat :: (BytesContainer c) => Int -> c -> JFloat
jfloat idx c = wordToFloat $ fetchBytes 4 idx c

jdouble :: (BytesContainer c) => Int -> c -> JDouble
jdouble idx c = wordToDouble $ fetchBytes 8 idx c

jreference :: (BytesContainer c) => Int -> c -> JReference
jreference = fetchBytes 8

-- Instructions DSL
type ThreadOperation a = StateT Thread IO a

getFrame :: Thread -> Frame
getFrame = (!! 0) . frames

updFrame :: Thread -> (Frame -> Frame) -> Thread
updFrame t@Thread {frames = (tframe:tframes)} f =
  t {frames = f tframe : tframes}

getStack :: Thread -> [StackElement]
getStack = operands . getFrame

updStack :: Thread -> ([StackElement] -> [StackElement]) -> Thread
updStack t@Thread {frames = (tframe@Frame {operands = toperands}:tframes)} f =
  t {frames = (tframe {operands = f toperands}) : tframes}

getLocals :: Thread -> Locals
getLocals = locals . getFrame

updLocals :: Thread -> (Array Int Word32 -> Array Int Word32) -> Thread
updLocals t@Thread {frames = (tframe@Frame {locals = Locals {vars = tvars}}:tframes)} f =
  t {frames = (tframe {locals = Locals $ f tvars}) : tframes}

remove :: ThreadOperation ()
remove = state $ \t -> ((), updStack t $ drop 1)

peek :: (JType j) => (Int -> StackElement -> j) -> ThreadOperation j
peek f = state $ \t -> (f 0 . (!! 0) . getStack $ t, t)

push :: (JType j) => j -> ThreadOperation ()
push j =
  state $ \t ->
    let elem = jToStackElement j
     in ((), updStack t (elem :))

popAndStoreAt ::
     (JType j, Show j)
  => (Int -> StackElement -> j)
  -> JLocalRef
  -> ThreadOperation ()
popAndStoreAt jaccess idx = do
  op <- pop jaccess
  store op idx

loadAndPushAt ::
     (JType j) => (Int -> Locals -> j) -> JLocalRef -> ThreadOperation ()
loadAndPushAt jaccess idx = do
  op <- load jaccess idx
  push op

jToStackElement :: (JType j) => j -> StackElement
jToStackElement j =
  StackElement $
  case represent j of
    Narrow x -> fromIntegral x
    Wide x -> x

arg :: (JType j) => (Int -> Locals -> j) -> Int -> ThreadOperation j
arg f n = state $ \t -> (f n $ getLocals t, t)

pushn :: (JType j) => [j] -> ThreadOperation ()
pushn js =
  state $ \t ->
    let vals = map jToStackElement js
     in ((), updStack t (vals ++))

popn :: (JType j) => (Int -> StackElement -> j) -> Int -> ThreadOperation [j]
popn f n =
  state $ \t ->
    let nElems = take n $ getStack t
     in (map (f 0) nElems, updStack t $ drop n)

pop :: (JType j) => (Int -> StackElement -> j) -> ThreadOperation j
pop f =
  state $ \t ->
    let nElems = head (getStack t)
     in (f 0 nElems, updStack t $ drop 1)

dropTopFrame :: ThreadOperation ()
dropTopFrame = state $ \t@Thread {frames = (f:fs)} -> ((), t {frames = fs})

store :: (JType j) => j -> JLocalRef -> ThreadOperation ()
store j idx =
  state $ \t ->
    let idx2 = fromIntegral idx
        arr = vars $ getLocals t
        newLocals =
          case represent j of
            Narrow x -> arr // [(idx2, x)]
            Wide x ->
              let (a, b) = split x
               in arr // [(idx2, a), (idx2 + 1, b)]
     in ((), updLocals t $ const newLocals)

split :: Word64 -> (Word32, Word32)
split x =
  let b = fromIntegral x
      a = fromIntegral $ rotate x 32
   in (a, b)

load :: (JType j) => (Int -> Locals -> j) -> JLocalRef -> ThreadOperation j
load f idx = state $ \t -> (f (fromIntegral idx) $ getLocals t, t)

signExtend :: (Integral a) => a -> JInt
signExtend = fromIntegral

empty :: ThreadOperation ()
empty = state $ \t -> ((), t)
