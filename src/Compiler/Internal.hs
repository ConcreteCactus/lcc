{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Compiler.Internal where

import qualified Data.List as Li
import Errors
import qualified Lexer as L
import SemanticAnalyzer
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
  deriving (Show)

data Closure = Closure
  { clCaptures :: [Int],
    clExpression :: Expression,
    clName :: CCode
  }
  deriving (Show)

compile :: S.Program -> CCode
compile program = runtime ++ unlines (map helper $ S.progDefs program)
  where
    helper def@(S.Definition gname _) = genFunction gname $ mkExpression def

showExpression :: L.Ident -> Expression -> (CCode, CCode)
showExpression gname expr =
  let (ExprBuildr statms gstatms _, expr') =
        runState (showExpressionS gname expr) (ExprBuildr [] [] 1)
   in ( unlines (map ("\t" ++) statms ++ ["\treturn " ++ expr' ++ ";"]),
        unlines gstatms
      )

showExpressionS :: L.Ident -> Expression -> State ExpressionBuilder CCode
showExpressionS _ ParamRef = return "param"
showExpressionS _ (Literal lit) = return $ show lit
showExpressionS _ (CaptureRef n) = return $ "self->capture_" ++ show n
showExpressionS _ (FunctionRef func) = return $ show func ++ "()"
showExpressionS gname (ClosureExpr closure) = addClosure gname closure
showExpressionS gname (Application expr1 expr2) = do
  expr1' <- showExpressionS gname expr1
  expr2' <- showExpressionS gname expr2
  return $ expr1' ++ "(" ++ expr2' ++ ")"

genFunction :: L.Ident -> Expression -> CCode
genFunction name expr =
  gcode
    ++ "\n"
    ++ "void* "
    ++ show name
    ++ "_func(void) {\n"
    ++ expr'
    ++ "}\n"
  where
    (expr', gcode) = showExpression name expr

addClosureFunction ::
  L.Ident ->
  CCode ->
  Expression ->
  State ExpressionBuilder CCode
addClosureFunction gname structName expr = do
  ind <- incBuilderIndex
  expr' <- separateBuilder $ showExpressionS gname expr
  let fnname = "clfunc_" ++ show gname ++ "_" ++ show ind
  let fncode =
        "void* "
          ++ fnname
          ++ "("
          ++ structName
          ++ "* self, void* param) {\n"
          ++ expr'
          ++ "\n}\n"
  addGlobalStatement fncode
  return fnname

addClosureStruct :: L.Ident -> [Int] -> State ExpressionBuilder CCode
addClosureStruct gname cptrs = do
  ind <- incBuilderIndex
  let structName = "clstruct_" ++ show gname ++ "_" ++ show ind
  let structCode =
        "typedef struct {\n"
          ++ "\tvoid* clfunc;\n"
          ++ concatMap ((++ ";\n") . ("\tvoid* capture_" ++) . show) cptrs
          ++ "} "
          ++ structName
          ++ ";\n"
  addGlobalStatement structCode
  return structName

addClosure :: L.Ident -> Closure -> State ExpressionBuilder CCode
addClosure gname (Closure cptrs expr _) = do
  ind <- incBuilderIndex
  clstruct <- addClosureStruct gname cptrs
  clfunc <- addClosureFunction gname clstruct expr
  addStatement $
    clstruct
      ++ "* c"
      ++ show ind
      ++ " = ("
      ++ clstruct
      ++ "*)new_closure(sizeof("
      ++ clstruct
      ++ "));"
  mapM_ (addStatement . assignHelper ind) cptrs
  addStatement $ "c" ++ show ind ++ "->clfunc = " ++ clfunc ++ ";"
  return $ "c" ++ show ind
  where
    assignHelper ind n =
      "c"
        ++ show ind
        ++ "->capture_"
        ++ show n
        ++ " = "
        ++ (if n > 2 then "self->capture_" ++ show (n - 1) else "param")
        ++ ";"

incBuilderIndex :: State ExpressionBuilder Int
incBuilderIndex = do
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

separateBuilder ::
  State ExpressionBuilder CCode ->
  State ExpressionBuilder CCode
separateBuilder builderS = do
  ExprBuildr statms gstatms ctr <- get
  let (ExprBuildr statms' gstatms' ctr', code) =
        runState builderS (ExprBuildr [] [] ctr)
  put (ExprBuildr statms (gstatms' ++ gstatms) ctr')
  return $ unlines (map ("\t" ++) statms') ++ "\treturn " ++ code ++ ";"

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
  return (ClosureExpr closure, cptrs)
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
  let cptrs' = filter (> 1) cptrs
  let closure =
        Closure
          { clCaptures = cptrs',
            clExpression = expr',
            clName = show ident ++ "_c_" ++ show clId
          }
  return (closure, map (+ (-1)) cptrs')

incState :: State Int Int
incState = do
  state <- get
  put $ state + 1
  return state

compileFull :: SourceCode -> Either CompilerError CCode
compileFull sc = do
  scy <- Y.parseProgramSingleError sc
  scs <- S.mkProgramFromSyn scy
  return $ compile scs

runtime :: CCode
runtime =
  "#include <stdlib.h>\n\n"
    ++ "void* new_closure(size_t size) {\n"
    ++ "\treturn malloc(size);\n"
    ++ "}\n\n"
