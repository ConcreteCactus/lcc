module StandardLibrary (
  standardLibrary,
  cTypeOf,
  StdCompilerDefinition (..),
) where

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

data StdCompilerDefinition a
  = ScdSimple ((Int -> Writer a String) -> Writer a [String])
  | ScdLiteral [String]

standardLibrary ::
  (Monoid a) =>
  [ ( L.Ident
    , T.NormType
    , StdCompilerDefinition a
    )
  ]
standardLibrary =
  map
    (\(idnt, typ, comp) -> (L.Ident idnt, T.mkNormType typ, comp))
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
  [(String, T.Type, StdCompilerDefinition a)]
makeTypedDefs (name, t, d) =
  map
    (\at -> (name ++ "_" ++ typeNameOf at, t at, ScdSimple $ d at))

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
  [(String, T.Type, StdCompilerDefinition a)]
library' =
  makeTypedDefs
    ( "add"
    , \t -> a t `to` a t `to` a t
    , \t w ->
        sequence
          [ p (cTypeOf t) <> p "* s1 = " <> w 1
          , p (cTypeOf t) <> p "* s2 = " <> w 2
          , p (cTypeOf t) <> p "* s3 = new_literal(sizeof(" <> p (cTypeOf t) <> p "))"
          , p "*s3 = (*s1) + (*s2)"
          , p "s3"
          ]
    )
    allAtomicTypes
    ++ makeTypedDefs
      ( "print"
      , \t -> a t `to` g 1 `to` g 1
      , \t w ->
          sequence
            [ p (cTypeOf t) <> p "* s1 = " <> w 1
            , p "printf(\"%" <> p (printfFormatStringBasedOnType t) <> p "\", (*s1)" <> p ")"
            , w 2
            ]
      )
      (allAtomicTypes `except` all128BitTypes)
    ++ makeTypedDefs
      ( "iseq"
      , \t -> a t `to` a t `to` a ABool
      , \t w ->
          sequence
            [ p (cTypeOf t) <> p "* s1 = " <> w 1
            , p (cTypeOf t) <> p "* s2 = " <> w 2
            , p (cTypeOf t) <> p "* s3 = new_literal(sizeof(" <> p (cTypeOf t) <> p "))"
            , p "*s3 = (*s1) == (*s2)"
            , p "s3"
            ]
      )
      allAtomicTypes
    ++ [
         ( "ifthenelse"
         , a ABool `to` g 1 `to` g 1 `to` g 1
         , ScdLiteral 
            [ "typedef struct {"
            , "\tgen_closure_clfunc* clfunc;"
            , "\tchar isLazy;"
            , "} ifthenelse_closure_true_1_t;"
            , ""
            , "typedef struct {"
            , "\tgen_closure_clfunc* clfunc;"
            , "\tchar isLazy;"
            , "\tvoid* capture_2;"
            , "} ifthenelse_closure_true_2_t;"
            , ""
            , "void* ifthenelse_clfunc_true_2(ifthenelse_closure_true_2_t* self, void* param) {"
            , "\treturn self->capture_2;"
            , "}"
            , ""
            , "void* ifthenelse_clfunc_true_1(ifthenelse_closure_true_1_t* self, void* param) {"
            , "\tifthenelse_closure_true_2_t* cl1 = new_closure(sizeof(*cl1));"
            , "\tcl1->isLazy = 1;"
            , "\tcl1->clfunc = (gen_closure_clfunc*)ifthenelse_clfunc_true_2;"
            , "\tcl1->capture_2 = param;"
            , "\treturn cl1;"
            , "}"
            , ""
            , "typedef struct {"
            , "\tgen_closure_clfunc* clfunc;"
            , "\tchar isLazy;"
            , "} ifthenelse_closure_false_1_t;"
            , ""
            , "typedef struct {"
            , "\tgen_closure_clfunc* clfunc;"
            , "\tchar isLazy;"
            , "} ifthenelse_closure_false_2_t;"
            , ""
            , "void* ifthenelse_clfunc_false_2(ifthenelse_closure_true_2_t* self, void* param) {"
            , "\treturn param;"
            , "}"
            , ""
            , "void* ifthenelse_clfunc_false_1(ifthenelse_closure_false_1_t* self, void* param) {"
            , "\tifthenelse_closure_false_2_t* cl1 = new_closure(sizeof(*cl1));"
            , "\tcl1->isLazy = 0;"
            , "\tcl1->clfunc = (gen_closure_clfunc*)ifthenelse_clfunc_false_2;"
            , "\treturn cl1;"
            , "}"
            , ""
            , "typedef struct {"
            , "\tgen_closure_clfunc* clfunc;"
            , "\tchar isLazy;"
            , "} ifthenelse_closure_def_t;"
            , ""
            , "void* ifthenelse_clfunc_1(ifthenelse_closure_def_t* self, void* param) {"
            , "\tif(*((char*)param)){"
            , "\t\tifthenelse_closure_true_1_t* cl1 = new_closure(sizeof(*cl1));"
            , "\t\tcl1->clfunc = (gen_closure_clfunc*)ifthenelse_clfunc_true_1;"
            , "\t\tcl1->isLazy = 0;"
            , "\t\treturn cl1;"
            , "\t} else {"
            , "\t\tifthenelse_closure_false_1_t* cl1 = new_closure(sizeof(*cl1));"
            , "\t\tcl1->clfunc = (gen_closure_clfunc*)ifthenelse_clfunc_false_1;"
            , "\t\tcl1->isLazy = 1;"
            , "\t\treturn cl1;"
            , "\t}"
            , "}"
            , ""
            , "void* ifthenelse_func(void) {"
            , "\tifthenelse_closure_def_t* cl1 = new_closure(sizeof(*cl1));"
            , "\tcl1->clfunc = (gen_closure_clfunc*)ifthenelse_clfunc_1;"
            , "\tcl1->isLazy = 0;"
            , "\treturn cl1;"
            , "}"
            ]
         )
       ]
