module StandardLibrary (library) where

import Data.Bifunctor
import qualified Lexer as L
import qualified SemanticAnalyzer.Type as T
import SyntacticAnalyzer

library :: [(L.Ident, T.NormType)]
library = map (bimap L.Ident T.mkNormType) library'

library' :: [(String, T.Type)]
library' =
  [ ("add_i8", a AI8 `to` a AI8 `to` a AI8)
  , ("print_i8", a AI8 `to` g 1 `to` g 1)
  ]

to :: T.Type -> T.Type -> T.Type
to = T.FunctionType

infixr 5 `to`

a :: AtomicType -> T.Type
a = T.AtomicType
g :: Int -> T.Type
g = T.GenericType
