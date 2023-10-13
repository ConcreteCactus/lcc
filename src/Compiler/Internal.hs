{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Compiler.Internal where

import qualified Data.List.NonEmpty as Ne
import qualified Lexer as L
import qualified SemanticAnalyzer as S
import qualified SemanticAnalyzer.Expression as SE
import qualified SemanticAnalyzer.Type as ST
import qualified SyntacticAnalyzer as Y
import Util

type CCode = String

type ClosureId = (L.Ident, Int)

type ClosureTable = [(ClosureId, Closure)]

data Expression
  = ClosureRef ClosureId
  | CaptureRef Int
  | Application Expression Expression
  | Literal Y.Literal

data Closure = Closure
  { clCaptureCount :: Int,
    clExpression :: Expression
  }

compile :: S.Program -> CCode
compile program = _

mkExpression :: S.Definition -> (Expression, ClosureTable)
mkExpression def = _

mkExpressionS ::
  L.Ident ->
  SE.Expression ->
  State Int (Expression, ClosureTable, [Int])
mkExpressionS _ (SE.Ident n) = return (CaptureRef n, [], [n])
mkExpressionS _ (SE.Ref n) = return (ClosureRef (n, 0), [], [])
mkExpressionS _ (SE.Lit l) = return (Literal l, [], [])
mkExpressionS ident (SE.Lambda _ expr) = do
  (closure, cltbl) <- mkClosure ident expr
  clid <- incState
  return (ClosureRef (ident, clid), ((ident, clid), closure) : cltbl, [])
mkExpressionS ident (SE.Application expr1 expr2) = do
  (expr1', cltbl1, cptrs1) <- mkExpressionS ident expr1
  (expr2', cltbl2, cptrs2) <- mkExpressionS ident expr2
  return (Application expr1' expr2', cltbl1 ++ cltbl2, cptrs1 +-+ cptrs2)

mkClosure :: L.Ident -> SE.Expression -> State Int (Closure, ClosureTable)
mkClosure ident expr = do
  (expr', cltbl, cptrs) <- mkExpressionS ident expr
  let closure =
        Closure
          { clCaptureCount = length cptrs,
            clExpression = expr'
          }
  return (closure, cltbl)

incState :: State Int Int
incState = do
  state <- get
  put $ state + 1
  return state
