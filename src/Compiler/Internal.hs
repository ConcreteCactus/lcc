{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Compiler.Internal where

import qualified Lexer as L
import qualified SemanticAnalyzer as S
import qualified SemanticAnalyzer.Expression as SE
import qualified SyntacticAnalyzer as Y
import Util
import Data.List

type CCode = String

newtype ClosureId = ClosureId (L.Ident, Int) deriving (Eq)

instance Show ClosureId where
  show (ClosureId (ident, n)) = show ident ++ "_" ++ show n

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
compile program = intercalate "\n" $ map buildClosure cltbl
  where
  exprs = map mkExpression (S.progDefs program)
  cltbl = concatMap snd exprs


instance Show Expression where
  show (ClosureRef clid) = show clid
  show (CaptureRef capid) = "param->p" ++ show capid
  show (Literal lit) = show lit
  show (Application exp1 exp2) = show exp1 ++ "(" ++ show exp2 ++ ")"

buildClosure :: (ClosureId, Closure) -> CCode
buildClosure (clid, Closure cc cexpr) = "typedef struct {\n" ++
  "void* clfunc;" ++
  intercalate "\n" ["void* p" ++ show n | n <- [1..cc]] ++
  "} " ++ show clid ++ ";" ++
  "void* " ++ show clid ++ "_clfunc(void* param){" ++
  show cexpr ++
  "}"

mkExpression :: S.Definition -> (Expression, ClosureTable)
mkExpression def =
  let (expr, cltbl, _) = execState (mkExpressionS (S.defName def) (S.teExpr (S.defExpr def))) 1
   in (expr, cltbl)

mkExpressionS ::
  L.Ident ->
  SE.Expression ->
  State Int (Expression, ClosureTable, [Int])
mkExpressionS _ (SE.Ident n) = return (CaptureRef n, [], [n])
mkExpressionS _ (SE.Ref n) = return (ClosureRef (ClosureId (n, 0)), [], [])
mkExpressionS _ (SE.Lit l) = return (Literal l, [], [])
mkExpressionS ident (SE.Lambda _ expr) = do
  (closure, cltbl) <- mkClosure ident expr
  clid <- incState
  return
    ( ClosureRef (ClosureId (ident, clid)),
      (ClosureId (ident, clid), closure) : cltbl,
      []
    )
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
