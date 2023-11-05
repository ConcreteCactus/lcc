module StandardLibrary (standardLibrary) where

import Data.Bifunctor
import qualified Lexer as L
import qualified SemanticAnalyzer.Type as T
import SyntacticAnalyzer

standardLibrary :: [(L.Ident, T.NormType)]
standardLibrary = map (bimap L.Ident T.mkNormType) library'

library' :: [(String, T.Type)]
library' =
  [ ("add_i32", a AI32 `to` a AI32 `to` a AI32)
  , ("print_i32", a AI32 `to` g 1 `to` g 1)
  ]

to :: T.Type -> T.Type -> T.Type
to = T.FunctionType

infixr 5 `to`

a :: AtomicType -> T.Type
a = T.AtomicType
g :: Int -> T.Type
g = T.GenericType
