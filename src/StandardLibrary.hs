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
cTypeOf AUSize = "usize_t"
cTypeOf AF32 = "float"
cTypeOf AF64 = "double"
cTypeOf AChar = "char"

standardLibrary ::
  (Monoid a) =>
  [ ( L.Ident
    , T.NormType
    , (Int -> Writer a String) -> Writer a [String]
    )
  ]
standardLibrary =
  map
    (\(idnt, typ, comp) -> (L.Ident idnt, T.mkNormType typ, comp))
    library'

p :: (Applicative m) => a -> m a
p = pure

library' ::
  (Monoid a) =>
  [(String, T.Type, (Int -> Writer a String) -> Writer a [String])]
library' =
  [
    ( "add_i32"
    , a AI32 `to` a AI32 `to` a AI32
    , \w ->
        sequence
          [ p (cTypeOf AI32) <> p "* s1 = " <> w 1
          , p (cTypeOf AI32) <> p "* s2 = " <> w 2
          , p (cTypeOf AI32) <> p "* s3 = new_literal(sizeof(" <> p (cTypeOf AI32) <> p "))"
          , p "*s3 = (*s1) + (*s2)"
          , p "s3"
          ]
    )
  ,
    ( "print_i32"
    , a AI32 `to` g 1 `to` g 1
    , \w ->
        sequence
          [ p (cTypeOf AI32) <> p "* s1 = " <> w 1
          , p "printf(\"%i\", (*s1)" <> p ")"
          , w 2
          ]
    )
  ]

to :: T.Type -> T.Type -> T.Type
to = T.FunctionType

infixr 5 `to`

a :: AtomicType -> T.Type
a = T.AtomicType
g :: Int -> T.Type
g = T.GenericType
