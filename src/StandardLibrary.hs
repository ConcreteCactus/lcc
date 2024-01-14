module StandardLibrary (standardLibrary, cTypeOf) where

import qualified Lexer as L
import qualified SemanticAnalyzer.Type as T
import SyntacticAnalyzer
import Util

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

standardLibrary ::
  (Monoid a) =>
  [ ( L.VarIdent
    , T.NormType
    , (Int -> Writer a String) -> Writer a [String]
    )
  ]
standardLibrary =
  map
    (\(idnt, typ, comp) -> (L.VarIdent idnt, T.mkNormType typ, comp))
    library'

p :: (Applicative m) => a -> m a
p = pure

to :: T.Type -> T.Type -> T.Type
to = T.FunctionType

infixr 5 `to`

a :: AtomicType -> T.Type
a = T.AtomicType
g :: Int -> T.Type
g = T.GenericType

{- FOURMOLU_DISABLE -}
allAtomicTypes :: [AtomicType]
allAtomicTypes =
  [ AI8, AI16, AI32, AI64, AI128
  , AU8, AU16, AU32, AU64, AU128, AUSize
  , AF32, AF64, AChar, ABool
  ]
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
allIntegerTypes :: [AtomicType]
allIntegerTypes =
  [ AI8, AI16, AI32, AI64, AI128
  , AU8, AU16, AU32, AU64, AU128, AUSize
  ]
{- FOURMOLU_ENABLE -}

allFloatTypes :: [AtomicType]
allFloatTypes = [AF32, AF64]

all128BitTypes :: [AtomicType]
all128BitTypes = [AI128, AU128]

makeTypedDefs ::
  ( String
  , AtomicType -> T.Type
  , AtomicType -> (Int -> Writer a String) -> Writer a [String]
  ) ->
  [AtomicType] ->
  [(String, T.Type, (Int -> Writer a String) -> Writer a [String])]
makeTypedDefs (name, t, d) =
  map
    (\at -> (name ++ "_" ++ typeNameOf at, t at, d at))

printfFormatStringBasedOnType :: AtomicType -> String
printfFormatStringBasedOnType t
  | t `elem` [AI8, AI16, AI32, ABool] = "i"
  | t == AI64 = "li"
  | t == AI128 = "lli"
  | t `elem` [AU8, AU16, AU32] = "u"
  | t `elem` [AU64, AUSize] = "lu"
  | t == AU128 = "llu"
  | t `elem` [AF32, AF64] = "f"
  | t == AChar = "c"
  | otherwise = "x"

library' ::
  (Monoid a) =>
  [(String, T.Type, (Int -> Writer a String) -> Writer a [String])]
library' =
  makeTypedDefs
    ( "add"
    , \t -> a t `to` a t `to` a t
    , \t w ->
        sequence
          [ p "literal* s1 = " <> w 1
          , p "literal* s2 = " <> w 2
          , p "literal* s3 = new_literal(sizeof(" <> p (cTypeOf t) <> p "))"
          , p "void* s1data = &s1->data"
          , p "void* s2data = &s2->data"
          , p "void* s3data = &s3->data"
          , p (cTypeOf t) <> p "* s3datai = s3data"
          , p "*s3datai = *(" <> p (cTypeOf t) <> p "*)s1data + *(" <> p (cTypeOf t) <> p "*)s2data"
          , p "s3->gc_data.isInStackSpace = 0;"
          , p "s3"
          ]
    )
    allAtomicTypes
    ++ makeTypedDefs
      ( "print"
      , \t -> a t `to` g 1 `to` g 1
      , \t w ->
          sequence
            [ p "literal* s1 = " <> w 1
            , p "void* s1data = &s1->data" 
            , p (cTypeOf t) <> p "* s1datai = s1data"
            , p "printf(\"%" <> p (printfFormatStringBasedOnType t) <> p "\", (*s1datai)" <> p ")"
            , w 2
            ]
      )
      (allAtomicTypes `except` all128BitTypes)
    ++ makeTypedDefs
      ( "iseq"
      , \t -> a t `to` a t `to` a ABool
      , \t w ->
          sequence
            [ p "literal* s1 = " <> w 1
            , p "literal* s2 = " <> w 2
            , p "literal* s3 = new_literal(sizeof(" <> p (cTypeOf t) <> p "))"
            , p "void* s1data = &s1->data"
            , p "void* s2data = &s2->data"
            , p "s3->data[0] = *(" <> p (cTypeOf t) <> p "*)s1data == *(" <> p (cTypeOf t) <> p "*)s2data"
            , p "s3->gc_data.isInStackSpace = 0;"
            , p "s3"
            ]
      )
      allAtomicTypes
