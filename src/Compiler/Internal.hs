{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Compiler.Internal where

import qualified Data.List as Li
import qualified Lexer as L
import qualified SemanticAnalyzer as S
import qualified SemanticAnalyzer.Expression as SE
import qualified SyntacticAnalyzer as Y
import Util

type CCode = String

data ExpressionBuilder = ExprBuildr
  { ebStatms :: [CCode],
    ebGStatms :: [CCode],
    ebCounter :: Int
  }

data Expression
  = ClosureExpr Closure
  | FunctionRef L.Ident
  | CaptureRef Int
  | ParamRef
  | Application Expression Expression
  | Literal Y.Literal

data Closure = Closure
  { clCaptures :: [Int],
    clExpression :: Expression,
    clName :: CCode
  }

compile :: S.Program -> CCode
compile program = _

instance Show Expression where
  show (ClosureExpr (Closure _ _ name)) = name
  show (FunctionRef ident) = show ident
  show (CaptureRef capid) = "self->p" ++ show capid
  show ParamRef = "param"
  show (Literal lit) = show lit
  show (Application exp1 exp2) = show exp1 ++ "(" ++ show exp2 ++ ")"

showExpressionS :: Expression -> State ExpressionBuilder CCode
showExpressionS ParamRef = return "param"
showExpressionS (Literal lit) = return $ show lit
showExpressionS (Application expr1 expr2) = do
  expr1' <- showExpressionS expr1
  expr2' <- showExpressionS expr2
  return $ expr1' ++ "(" ++ expr2' ++ ")"
showExpressionS (CaptureRef n) = return $ "self->capture_" ++ show n
showExpressionS (FunctionRef func) = return $ show func ++ "()"
showExpressionS (ClosureExpr closure) = addClosure closure

addClosure :: Closure -> State ExpressionBuilder CCode
addClosure (Closure cptrs expr name) = do
  ind <- incClosureIndex
  addStatement $ name ++ "* c" ++ show ind ++ ";\n"
  mapM_ (addStatement . assignHelper ind) cptrs
  -- also add clfunction and call showExpressionS on the body
  -- also add struct
  return $ "c" ++ show ind
  where
    assignHelper ind n =
      "c"
        ++ show ind
        ++ "->capture_"
        ++ show n
        ++ " = "
        ++ (if n > 1 then "self->capture_" ++ show (n - 1) else "param")

incClosureIndex :: State ExpressionBuilder Int
incClosureIndex = do
  ExprBuildr statms gstatms ind <- get
  put $ ExprBuildr statms gstatms (ind + 1)
  return ind

addStatement :: CCode -> State ExpressionBuilder ()
addStatement code = do
  ExprBuildr statms gstatms ind <- get
  put $ ExprBuildr (statms ++ [code]) gstatms ind

addGlobalStatement :: CCode -> State ExpressionBuilder ()
addGlobalStatement code = do
  ExprBuildr statms gstatms ind <- get
  put $ ExprBuildr statms (gstatms ++ [code]) ind

buildClosure :: Closure -> CCode
buildClosure (Closure cptrs expr name) =
  "typedef struct {\n"
    ++ "\tvoid* clfunc;"
    ++ Li.intercalate ";\n\t" (map (("void* capture_" ++) . show) cptrs)
    ++ ";\n} "
    ++ name
    ++ ";"
    ++ "void* "
    ++ name
    ++ "_clfunc("
    ++ name
    ++ "* self, void* param){\n"
    ++ show expr
    ++ ";\n"
    ++ "}"

mkExpression :: S.Definition -> Expression
mkExpression def =
  let (expr, _) =
        execState
          (mkExpressionS (S.defName def) (S.teExpr (S.defExpr def)))
          1
   in expr

mkExpressionS ::
  L.Ident ->
  SE.Expression ->
  State Int (Expression, [Int])
mkExpressionS _ (SE.Ident n) | n == 1 = return (ParamRef, [n])
mkExpressionS _ (SE.Ident n) = return (CaptureRef n, [n])
mkExpressionS _ (SE.Ref n) = return (FunctionRef n, [])
mkExpressionS _ (SE.Lit l) = return (Literal l, [])
mkExpressionS ident (SE.Lambda _ expr) = do
  (closure, cptrs) <- mkClosure ident expr
  return (ClosureExpr closure, map (+ (-1)) $ filter (> 1) cptrs)
mkExpressionS ident (SE.Application expr1 expr2) = do
  (expr1', cptrs1) <- mkExpressionS ident expr1
  (expr2', cptrs2) <- mkExpressionS ident expr2
  return (Application expr1' expr2', Li.sort (cptrs1 +-+ cptrs2))

mkClosure ::
  L.Ident ->
  SE.Expression ->
  State Int (Closure, [Int])
mkClosure ident expr = do
  (expr', cptrs) <- mkExpressionS ident expr
  clId <- incState
  let closure =
        Closure
          { clCaptures = cptrs,
            clExpression = expr',
            clName = show ident ++ "_c_" ++ show clId
          }
  return (closure, cptrs)

incState :: State Int Int
incState = do
  state <- get
  put $ state + 1
  return state
