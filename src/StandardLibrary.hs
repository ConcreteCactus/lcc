{-# LANGUAGE ImpredicativeTypes #-}
module StandardLibrary (standardLibrary) where

import qualified Lexer as L
import qualified SemanticAnalyzer.Type as T
import SyntacticAnalyzer

standardLibrary :: [(L.Ident, T.NormType, (Show s) => (Int -> s) -> [String])]
standardLibrary =
  map
    (\(idnt, typ, comp) -> (L.Ident idnt, T.mkNormType typ, comp))
    library'

library' :: [(String, T.Type, (Show s) => (Int -> s) -> [String])]
library' =
  [
    ( "add_i32"
    , a AI32 `to` a AI32 `to` a AI32
    , \p -> [p $^ 1 ++ " + " ++ p $^ 2]
    )
  ,
    ( "print_i32"
    , a AI32 `to` g 1 `to` g 1
    , \p -> ["printf(\"%i\", " ++ p $^ 1 ++ ")", p $^ 2]
    )
  ]

($^) :: (Show s) => (a -> s) -> a -> String
f $^ a' = show (f a')

infixr 6 $^

to :: T.Type -> T.Type -> T.Type
to = T.FunctionType

infixr 5 `to`

a :: AtomicType -> T.Type
a = T.AtomicType
g :: Int -> T.Type
g = T.GenericType
