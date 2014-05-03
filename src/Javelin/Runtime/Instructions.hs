module Javelin.Runtime.Instructions
where

import Javelin.Runtime.Thread (JByte, JShort, JInt, JLong, JBoolean, JChar, JDouble, JFloat, JRaw,
                               jbyte, jshort, jint, jlong, jboolean, jchar, jdouble, jfloat, jraw,
                               remove, peek, arg, push, pushn, pop, popn, store, load, signExtend,
                               )
import qualified Data.Map.Lazy as Map (fromList, Map)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Bits ((.|.), (.&.), xor)
import Javelin.Runtime.Thread (empty, Instruction(..))


type ArgumentsParser = [Word8] -> [Word8]
fixed :: Int -> ArgumentsParser
fixed = take

dummy :: ArgumentsParser
dummy = undefined

noarg :: ArgumentsParser
noarg = \_ -> []


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
istoreAt idx  = do
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
dup_x1 = do
  [op1, op2] <- popn jraw 2
  pushn [op1, op2, op1]
dup_x2 = do
  [op1, op2, op3] <- popn jraw 3
  pushn [op1, op3, op2, op1]
dup2 = do
  [op1, op2] <- popn jraw 2
  pushn [op2, op1, op2, op1]
dup2_x1 = do
  [op1, op2, op3] <- popn jraw 3
  pushn [op2, op1, op3, op2, op1]
dup2_x2 = do
  [op1, op2, op3, op4] <- popn jraw 4
  pushn [op2, op1, op4, op3, op2, op1]
swap = do
  [op1, op2] <- popn jraw 2
  pushn [op2, op1]


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
iinc = do
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
invokespecial = undefined
invokestatic = undefined
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

tableswitchArgs :: ArgumentsParser
tableswitchArgs = undefined

lookupswitchArgs :: ArgumentsParser
lookupswitchArgs = undefined

wideArgs :: ArgumentsParser
wideArgs = undefined

multianewarrayArgs :: ArgumentsParser
multianewarrayArgs = undefined

instructions :: Map.Map Word8 (ArgumentsParser, Instruction)
instructions = Map.fromList [
  
  -- Constants
  (0x00, (noarg, nop)),
  (0x01, (noarg, aconst_null)),
  (0x02, (noarg, iconst_m1)),
  (0x03, (noarg, iconst_0)),
  (0x04, (noarg, iconst_1)),
  (0x05, (noarg, iconst_2)),
  (0x06, (noarg, iconst_3)),
  (0x07, (noarg, iconst_4)),
  (0x08, (noarg, iconst_5)),
  (0x09, (noarg, lconst_0)),
  (0x0a, (noarg, lconst_1)),
  (0x0b, (noarg, fconst_0)),
  (0x0c, (noarg, fconst_1)),
  (0x0d, (noarg, fconst_2)),
  (0x0e, (noarg, dconst_0)),
  (0x0f, (noarg, dconst_1)),
  (0x10, (fixed 1, bipush)),
  (0x11, (fixed 2, sipush)),
  (0x12, (fixed 1, ldc)),
  (0x13, (fixed 2, ldc_w)),
  (0x14, (fixed 2, ldc2_w)),

  
  -- Loads
  (0x15, (fixed 1, iload)),
  (0x16, (fixed 1, lload)),
  (0x17, (fixed 1, fload)),
  (0x18, (fixed 1, dload)),
  (0x19, (fixed 1, aload)),
  (0x1a, (noarg, iload_0)),
  (0x1b, (noarg, iload_1)),
  (0x1c, (noarg, iload_2)),
  (0x1d, (noarg, iload_3)),
  (0x1e, (noarg, lload_0)),
  (0x1f, (noarg, lload_1)),
  (0x20, (noarg, lload_2)),
  (0x21, (noarg, lload_3)),
  (0x22, (noarg, fload_0)),
  (0x23, (noarg, fload_1)),
  (0x24, (noarg, fload_2)),
  (0x25, (noarg, fload_3)),
  (0x26, (noarg, dload_0)),
  (0x27, (noarg, dload_1)),
  (0x28, (noarg, dload_2)),
  (0x29, (noarg, dload_3)),
  (0x2a, (noarg, aload_0)),
  (0x2b, (noarg, aload_1)),
  (0x2c, (noarg, aload_2)),
  (0x2d, (noarg, aload_3)),
  (0x2e, (noarg, iaload)),
  (0x2f, (noarg, laload)),
  (0x30, (noarg, faload)),
  (0x31, (noarg, daload)),
  (0x32, (noarg, aaload)),
  (0x33, (noarg, baload)),
  (0x34, (noarg, caload)),
  (0x35, (noarg, saload)),
    
  
  -- Stores
  (0x36, (fixed 1, istore)),
  (0x37, (fixed 1, lstore)),
  (0x38, (fixed 1, fstore)),
  (0x39, (fixed 1, dstore)),
  (0x3a, (fixed 1, astore)),
  (0x3b, (noarg, istore_0)),
  (0x3c, (noarg, istore_1)),
  (0x3d, (noarg, istore_2)),
  (0x3e, (noarg, istore_3)),
  (0x3f, (noarg, lstore_0)),
  (0x40, (noarg, lstore_1)),
  (0x41, (noarg, lstore_2)),
  (0x42, (noarg, lstore_3)),
  (0x43, (noarg, fstore_0)),
  (0x44, (noarg, fstore_1)),
  (0x45, (noarg, fstore_2)),
  (0x46, (noarg, fstore_3)),
  (0x47, (noarg, dstore_0)),
  (0x48, (noarg, dstore_1)),
  (0x49, (noarg, dstore_2)),
  (0x4a, (noarg, dstore_3)),
  (0x4b, (noarg, astore_0)),
  (0x4c, (noarg, astore_1)),
  (0x4d, (noarg, astore_2)),
  (0x4e, (noarg, astore_3)),
  (0x4f, (noarg, iastore)),
  (0x50, (noarg, lastore)),
  (0x51, (noarg, fastore)),
  (0x52, (noarg, dastore)),
  (0x53, (noarg, aastore)),
  (0x54, (noarg, bastore)),
  (0x55, (noarg, castore)),
  (0x56, (noarg, sastore)),

  
  -- Stack
  (0x57, (noarg, pop1)),
  (0x58, (noarg, pop2)),
  (0x59, (noarg, dup)),
  (0x5a, (noarg, dup_x1)),
  (0x5b, (noarg, dup_x2)),
  (0x5c, (noarg, dup2)),
  (0x5d, (noarg, dup2_x1)),
  (0x5e, (noarg, dup2_x2)),
  (0x5f, (noarg, swap)),
  

  -- Math
  (0x60, (noarg, iadd)),
  (0x61, (noarg, ladd)),
  (0x62, (noarg, fadd)),
  (0x63, (noarg, dadd)),
  (0x64, (noarg, isub)),
  (0x65, (noarg, lsub)),
  (0x66, (noarg, fsub)),
  (0x67, (noarg, dsub)),
  (0x68, (noarg, imul)),
  (0x69, (noarg, lmul)),
  (0x6a, (noarg, fmul)),
  (0x6b, (noarg, dmul)),
  (0x6c, (noarg, idiv)),
  (0x6d, (noarg, ldiv)),
  (0x6e, (noarg, fdiv)),
  (0x6f, (noarg, ddiv)),
  (0x70, (noarg, irem)),
  (0x71, (noarg, lrem)),
  (0x72, (noarg, frem)),
  (0x73, (noarg, drem)),
  (0x74, (noarg, ineg)),
  (0x75, (noarg, lneg)),
  (0x76, (noarg, fneg)),
  (0x77, (noarg, dneg)),
  (0x78, (noarg, ishl)),
  (0x79, (noarg, lshl)),
  (0x7a, (noarg, ishr)),
  (0x7b, (noarg, lshr)),
  (0x7c, (noarg, iushr)),
  (0x7d, (noarg, lushr)),
  (0x7e, (noarg, iand)),
  (0x7f, (noarg, land)),
  (0x80, (noarg, ior)),
  (0x81, (noarg, lor)),
  (0x82, (noarg, ixor)),
  (0x83, (noarg, lxor)),
  (0x84, (fixed 2, iinc)),

  
  -- Conversions
  (0x85, (noarg, i2l)),
  (0x86, (noarg, i2f)),
  (0x87, (noarg, i2d)),
  (0x88, (noarg, l2i)),
  (0x89, (noarg, l2f)),
  (0x8a, (noarg, l2d)),
  (0x8b, (noarg, f2i)),
  (0x8c, (noarg, f2l)),
  (0x8d, (noarg, f2d)),
  (0x8e, (noarg, d2i)),
  (0x8f, (noarg, d2l)),
  (0x90, (noarg, d2f)),
  (0x91, (noarg, i2b)),
  (0x92, (noarg, i2c)),
  (0x93, (noarg, i2s)),

  
  -- Comparisons
  (0x94, (noarg, lcmp)),
  (0x95, (noarg, fcmpl)),
  (0x96, (noarg, fcmpg)),
  (0x97, (noarg, dcmpl)),
  (0x98, (noarg, dcmpg)),
  (0x99, (fixed 2, ifeq)),
  (0x9a, (fixed 2, ifne)),
  (0x9b, (fixed 2, iflt)),
  (0x9c, (fixed 2, ifge)),
  (0x9d, (fixed 2, ifgt)),
  (0x9e, (fixed 2, ifle)),
  (0x9f, (fixed 2, if_icmpeq)),
  (0xa0, (fixed 2, if_icmpne)),
  (0xa1, (fixed 2, if_icmplt)),
  (0xa2, (fixed 2, if_icmpge)),
  (0xa3, (fixed 2, if_icmpgt)),
  (0xa4, (fixed 2, if_icmple)),
  (0xa5, (fixed 2, if_acmpeq)),
  (0xa6, (fixed 2, if_acmpne)),

  
  -- Control
  (0xa7, (fixed 2, goto)),
  (0xa8, (fixed 2, jsr)),
  (0xa9, (fixed 1, ret)),
  (0xaa, (tableswitchArgs, tableswitch)),
  (0xab, (lookupswitchArgs, lookupswitch)),
  (0xac, (noarg, ireturn)),
  (0xad, (noarg, lreturn)),
  (0xae, (noarg, freturn)),
  (0xaf, (noarg, dreturn)),
  (0xb0, (noarg, areturn)),
  (0xb1, (noarg, _return)),

  
  -- References
  (0xb2, (fixed 2, getstatic)),
  (0xb3, (fixed 2, putstatic)),
  (0xb4, (fixed 2, getfield)),
  (0xb5, (fixed 2, putfield)),
  (0xb6, (fixed 2, invokevirtual)),
  (0xb7, (fixed 2, invokespecial)),
  (0xb8, (fixed 2, invokestatic)),
  (0xb9, (fixed 4, invokeinterface)),
  (0xba, (fixed 4, invokedynamic)),
  (0xbb, (fixed 2, new)),
  (0xbc, (fixed 1, newarray)),
  (0xbd, (fixed 2, anewarray)),
  (0xbe, (noarg, arraylength)),
  (0xbf, (noarg, athrow)),
  (0xc0, (fixed 2, checkcast)),
  (0xc1, (fixed 2, instanceof)),
  (0xc2, (noarg, monitorenter)),
  (0xc3, (noarg, monitorexit)),
  

  -- Extended
  (0xc4, (wideArgs, wide)),
  (0xc5, (multianewarrayArgs, multianewarray)),
  (0xc6, (fixed 2, ifnull)),
  (0xc7, (fixed 2, ifnonnull)),
  (0xc8, (fixed 4, goto_w)),
  (0xc9, (fixed 4, jsr_w)),


  -- Reserved
  (0xca, (noarg, breakpoint)),
  (0xfe, (noarg, impdep1)),
  (0xff, (noarg, impdep2))
 
  ]
