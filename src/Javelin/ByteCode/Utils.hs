module Javelin.ByteCode.Utils
where

import Data.ByteString (ByteString, unpack, pack)
import Data.Word (Word32, Word16, Word8)
import qualified Data.Map.Lazy as Map (findWithDefault, fromList, Map(..), keys, lookup)
import Data.Bits
import Data.Maybe
import Control.Applicative
import Data.Binary.Get
import Data.ByteString.UTF8 (toString)

import Javelin.ByteCode.Data

getByte = getWord8
getWord = getWord16be
getDWord = getWord32be

times :: Get a -> Word16 -> Get [a]
times _ 0 = return []
times get n = (:) <$> get <*> times get (n - 1)
    
several :: Get a -> Get [a]
several get = do
  len <- getWord
  times get len

addFlagIfMatches :: Word16 -> Map.Map Word16 a -> [a] -> Word16 -> [a]
addFlagIfMatches number flagsMap list mask = if (mask .&. number) == 0
                                             then list
                                             else case Map.lookup mask flagsMap of
                                               Just x -> x : list
                                               Nothing -> list

foldMask ::Map.Map Word16 a -> Word16 -> [a]
foldMask flagsMap bytes = foldl (addFlagIfMatches bytes flagsMap) [] (Map.keys flagsMap)

getFromPool :: [x] -> Word16 -> Maybe x
getFromPool list idx = if okIdx < length list
                       then Just $ list !! okIdx
                       else Nothing
  where okIdx = fromIntegral idx

bytesToString :: ByteString -> String
bytesToString = toString

type FullName = [String]
type UnqualifiedName = String

data FieldType = BaseType { baseType :: BaseType }
               | ObjectType { className :: FullName }
               | ArrayType { componentType :: FieldType }
               deriving (Show, Eq)
data BaseType = ByteT | CharT | DoubleT | FloatT | IntT | LongT | ShortT | BooleanT
              deriving (Show, Eq)


-- FieldDescriptor
data FieldDescriptor = FieldDescriptor { fieldType :: FieldType }
                     deriving (Show, Eq)

-- MethodDescriptor
data MethodDescriptor = MethodDescriptor { parameterDescrs :: [FieldType],
                                           returnDescr :: ReturnDescriptor }
                        deriving (Show, Eq)
data ReturnDescriptor = FieldType | VoidDescriptor deriving (Show, Eq)


-- ClassSignature
data ClassSignature = ClassSignature { formalTypeParameters :: [FormalTypeParameter],
                                       superclassSignature :: ClassTypeSignature,
                                       superinterfaceSignature :: ClassTypeSignature }
                    deriving (Show, Eq)
data FormalTypeParameter = FormalTypeParameter { identifier :: String,
                                                 classBound :: [FieldTypeSignature],
                                                 interfaceBound :: [FieldTypeSignature] }
                           deriving (Show, Eq)
data FieldTypeSignature = ClassTypeSignature
                        | ArrayTypeSignature
                        | TypeVariableSignature

                          
