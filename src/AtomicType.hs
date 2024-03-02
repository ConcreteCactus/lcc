{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AtomicType where

{- FOURMOLU_DISABLE -}
data AtomicType
  = AI8 | AI16 | AI32 | AI64 | AI128
  | AU8 | AU16 | AU32 | AU64 | AU128 | AUSize
  | AF32 | AF64 | AChar | ABool
  deriving (Show, Eq)
{- FOURMOLU_ENABLE -}

signedIntAtomicTypes :: [AtomicType]
signedIntAtomicTypes = [AI8, AI16, AI32, AI64, AI128]

unsignedIntAtomicTypes :: [AtomicType]
unsignedIntAtomicTypes = [AU8, AU16, AU32, AU64, AU128, AUSize, AChar, ABool]

floatAtomicTypes :: [AtomicType]
floatAtomicTypes = [AF32, AF64]
    

getTypeFromPostfix :: String -> Maybe AtomicType
getTypeFromPostfix name
    | name == "i8" = Just AI8
    | name == "i16" = Just AI16
    | name == "i32" = Just AI32
    | name == "i64" = Just AI64
    | name == "i128" = Just AI128
    | name == "u8" = Just AU8
    | name == "u16" = Just AU16
    | name == "u32" = Just AU32
    | name == "u64" = Just AU64
    | name == "u128" = Just AU128
    | name == "usize" = Just AUSize
    | name == "f32" = Just AF32
    | name == "f64" = Just AF64
    | name == "char" = Just AChar
    | name == "bool" = Just ABool
    | otherwise = Nothing

getTypeNameFromStr :: String -> Maybe AtomicType
getTypeNameFromStr name
    | name == "I8" = Just AI8
    | name == "I16" = Just AI16
    | name == "I32" = Just AI32
    | name == "I64" = Just AI64
    | name == "I128" = Just AI128
    | name == "U8" = Just AU8
    | name == "U16" = Just AU16
    | name == "U32" = Just AU32
    | name == "U64" = Just AU64
    | name == "U128" = Just AU128
    | name == "USize" = Just AUSize
    | name == "F32" = Just AF32
    | name == "F64" = Just AF64
    | name == "Char" = Just AChar
    | name == "Bool" = Just ABool
    | otherwise = Nothing

cTypeOf :: AtomicType -> String
cTypeOf AI8 = "int8_t"
cTypeOf AI16 = "int16_t"
cTypeOf AI32 = "int32_t"
cTypeOf AI64 = "int64_t"
cTypeOf AI128 = "int128_t"
cTypeOf AU8 = "uint8_t"
cTypeOf AU16 = "uint16_t"
cTypeOf AU32 = "uint32_t"
cTypeOf AU64 = "uint64_t"
cTypeOf AU128 = "uint128_t"
cTypeOf AUSize = "size_t"
cTypeOf AF32 = "float"
cTypeOf AF64 = "double"
cTypeOf AChar = "char"
cTypeOf ABool = "char"

typeNameOf :: AtomicType -> String
typeNameOf AI8 = "i8"
typeNameOf AI16 = "i16"
typeNameOf AI32 = "i32"
typeNameOf AI64 = "i64"
typeNameOf AI128 = "i128"
typeNameOf AU8 = "u8"
typeNameOf AU16 = "u16"
typeNameOf AU32 = "u32"
typeNameOf AU64 = "u64"
typeNameOf AU128 = "u128"
typeNameOf AUSize = "usize"
typeNameOf AF32 = "f32"
typeNameOf AF64 = "f64"
typeNameOf AChar = "char"
typeNameOf ABool = "bool"
