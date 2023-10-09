{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Compiler.Internal where

import qualified Data.List.NonEmpty as Ne
import qualified Lexer as L
import qualified SemanticAnalyzer as S
import qualified SemanticAnalyzer.Expression as SE
import qualified SemanticAnalyzer.Type as ST
import Util

type CCode = String

data Closure = Closure
  { clParamType :: ST.Type,
    clReturnType :: ST.Type,
    clCaptureType :: [ST.Type],
    clExpression :: SE.Expression,
    clId :: (L.Ident, Int)
  }

compile :: S.Program -> CCode
compile program = _

extractClosuresFromDef :: S.Definition -> [Closure]
extractClosuresFromDef def = execState (extractHelperS name (typ, expr)) 1
  where
    name = S.defName def
    typ = ST.ntType $ S.teType $ S.defExpr def
    expr = S.teExpr $ S.defExpr def

extractHelperS ::
  L.Ident ->
  [ST.Type] ->
  (ST.Type, SE.Expression) ->
  State Int (Ne.NonEmpty Closure)
extractHelperS ident captures (ST.FunctionType parT retT, SE.Lambda _ expr) =
  do
    ind <- incState
    let newId = (ident, ind)
    let newParamType = parT
    let newRetType = retT
    let newExpression = expr
    _
extractHelperS _ _ _ = error "This is not implemented yet!"

incState :: State Int Int
incState = do
  state <- get
  put $ state + 1
  return state
