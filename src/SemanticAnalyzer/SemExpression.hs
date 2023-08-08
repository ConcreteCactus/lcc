module SemanticAnalyzer.SemExpression (SemExpression (..)) where

import SyntacticAnalyzer
import Util

data SemExpression
  = SId Int
  | SRef String
  | SLit Literal
  | SLambda String SemExpression
  | SApplication SemExpression SemExpression
  deriving (Eq)

instance Show SemExpression where
  show = showHelper []

showHelper :: [String] -> SemExpression -> String
showHelper names (SId identifier) =
  case names !!! identifier of
    Nothing -> "?_" ++ show identifier
    Just name -> name ++ "_" ++ show identifier
showHelper _ (SRef name) = name
showHelper _ (SLit literal) = show literal
showHelper names (SLambda paramName expr) =
  "λ"
    ++ paramName
    ++ "."
    ++ showHelper (paramName : names) expr
showHelper names (SApplication expr1@(SLambda _ _) expr2@(SApplication _ _)) =
  "(" ++ showHelper names expr1 ++ ") (" ++ showHelper names expr2 ++ ")"
showHelper names (SApplication expr1@(SLambda _ _) expr2) =
  "(" ++ showHelper names expr1 ++ ") " ++ showHelper names expr2
showHelper names (SApplication expr1 expr2@(SApplication _ _)) =
  showHelper names expr1 ++ " (" ++ showHelper names expr2 ++ ")"
showHelper names (SApplication expr1 expr2) =
  showHelper names expr1 ++ " " ++ showHelper names expr2
