{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Compiler.Internal where

import qualified Data.List as Li
import Data.Maybe
import Errors
import qualified Lexer as L
import SemanticAnalyzer
import qualified SemanticAnalyzer as S
import qualified SemanticAnalyzer.Expression as SE
import StandardLibrary
import qualified SyntacticAnalyzer as Y
import Util

type CCode = String

data ExpressionBuilder = ExprBuildr
  { ebStatms :: [CCode]
  , ebGStatms :: [CCode]
  , ebCounter :: Int
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
  { clCaptures :: [Int]
  , clExpression :: Expression
  , clName :: CCode
  , clIsLazy :: Bool
  }
  deriving (Show)

compile :: S.Program -> CCode
compile program =
  includes
    ++ constPredefs
    ++ concatMap ((++ "\n") . predefHelper) (S.progDefs program)
    ++ runtime
    ++ stdDefinitions
    ++ unlines (map genHelper $ S.progDefs program)
    ++ if mainfnHelper then mainfn else mainfnEmpty
 where
  genHelper def@(S.Definition gname _) = genFunction gname $ mkExpression def
  predefHelper (S.Definition gname _) =
    "void* "
      ++ show gname
      ++ "_func(void);"
  mainfnHelper =
    isJust
      $ Li.find
        ( \(S.Definition gname _) ->
            show gname == "main"
        )
        (S.progDefs program)

showExpression :: L.Ident -> Expression -> (CCode, CCode)
showExpression gname expr =
  let (ExprBuildr statms gstatms _, expr') =
        runState (showExpressionS gname expr) (ExprBuildr [] [] 1)
   in ( unlines (map ("\t" ++) statms ++ ["\treturn " ++ expr' ++ ";"])
      , unlines gstatms
      )

{- FOURMOLU_DISABLE -}
showExpressionS :: L.Ident -> Expression -> State ExpressionBuilder CCode
showExpressionS _ ParamRef = return "param"
showExpressionS _ (Literal (Y.Literal typ lit)) = do
  lid <- incBuilderIndex
  let litName = "l" ++ show lid
  addStatement $ cTypeOf typ ++ "* " ++ litName ++ " = new_literal(sizeof(" ++ cTypeOf typ ++ "));"
  addStatement $ "*" ++ litName ++ " = " ++ show lit ++ ";"
  return litName
showExpressionS _ (CaptureRef n) = return $ "self->capture_" ++ show n
showExpressionS _ (FunctionRef func) = do
  return $ show func ++ "_func()"
showExpressionS gname (ClosureExpr closure) = addClosure gname closure
showExpressionS gname (Application expr1 expr2) = do
  expr1' <- showExpressionS gname expr1
  indx1 <- incBuilderIndex
  let ifl = "ifl" ++ show indx1
  addStatement $ "void* " ++ ifl ++ ";"
  indx2 <- incBuilderIndex
  let icl = "icl" ++ show indx2
  addStatement $ "gen_closure* " ++ icl ++ " = " ++ expr1' ++ ";"
  addStatement $ "if(!" ++ icl ++ "->isLazy){"
  expr2' <- showExpressionS gname expr2
  addStatement $ ifl ++ " = " ++ icl ++ "->clfunc(" ++ icl ++ ", " ++ expr2' ++ ");"
  addStatement "} else {"
  addStatement $ ifl ++ " = " ++ icl ++ "->clfunc(" ++ icl ++ ", NULL);"
  addStatement "}"
  return ifl
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
genFunction :: L.Ident -> Expression -> CCode
genFunction name expr =
  gcode ++ "\n" ++
  "void* " ++ show name ++ "_func(void) {\n" ++
        expr' ++
  "}\n"
 where
  (expr', gcode) = showExpression name expr
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
addClosureFunction ::
  L.Ident ->
  CCode ->
  Expression ->
  Bool ->
  State ExpressionBuilder CCode
addClosureFunction gname structName expr isLazy = do
  ind <- incBuilderIndex
  expr' <- separateBuilder $ showExpressionS gname expr
  let fnname = "clfunc_" ++ show gname ++ "_" ++ show ind
  let fncode = 
        if isLazy 
        then 
        "void* " ++ fnname ++ "(" ++ structName ++ "* self) {\n" ++
            expr' ++ "\n" ++
        "}\n"
        else 
        "void* " ++ fnname ++ "(" ++ structName ++ "* self, void* param) {\n" ++
            expr' ++ "\n" ++
        "}\n"
  addGlobalStatement fncode
  return fnname
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
addClosureStruct :: L.Ident -> [Int] -> State ExpressionBuilder CCode
addClosureStruct gname cptrs = do
  ind <- incBuilderIndex
  let structName = "clstruct_" ++ show gname ++ "_" ++ show ind
  let structCode =
        "typedef struct {\n" ++
            "\tvoid* clfunc;\n" ++
            "\tchar isLazy;\n" ++
            concatMap ((++ ";\n") . ("\tvoid* capture_" ++) . show) cptrs ++
        "} " ++ structName ++ ";\n"
  addGlobalStatement structCode
  return structName
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
addClosure :: L.Ident -> Closure -> State ExpressionBuilder CCode
addClosure gname (Closure cptrs expr _ isLazy) = do
  ind <- incBuilderIndex
  clstruct <- addClosureStruct gname cptrs
  clfunc <- addClosureFunction gname clstruct expr isLazy
  addStatement $
    clstruct ++ "* c" ++ show ind ++ " = (" ++ clstruct ++
    "*)new_closure(sizeof(" ++ clstruct ++ "));"
  mapM_ (addStatement . assignHelper ind) cptrs
  addStatement $ "c" ++ show ind ++ "->clfunc = " ++ clfunc ++ ";"
  addStatement $ "c" ++ show ind ++ "->isLazy = " ++ if isLazy then "1" else "0" ++ ";"
  return $ "c" ++ show ind
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
assignHelper :: Int -> Int -> CCode
assignHelper ind n = "c" ++ show ind ++ "->capture_" ++ show n ++ " = " ++
    (if n > 2 then "self->capture_" ++ show (n - 1) else "param") ++ ";"
{- FOURMOLU_ENABLE -}

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
          { clCaptures = cptrs'
          , clExpression = expr'
          , clName = show ident ++ "_c_" ++ show clId
          , clIsLazy = 1 `notElem` cptrs
          }
  return (closure, map (+ (-1)) cptrs')

incState :: State Int Int
incState = do
  state <- get
  put $ state + 1
  return state

compileFull :: SourceCode -> Either CompilerError CCode
compileFull sc = do
  scy <- leftMap mkCompErrLex $ Y.parseProgramSingleError sc
  scs <- S.mkProgramFromSyn scy
  return $ compile scs

{- FOURMOLU_DISABLE -}
includes :: CCode
includes = "#include <stdlib.h>\n#include <stdio.h>\n#include <stdint.h>\n\n"
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
runtime :: CCode
runtime =
  "typedef __int128 int128_t;\n" ++
  "typedef unsigned __int128 uint128_t;\n" ++
  "void* new_closure(size_t size) {\n" ++
      "\treturn malloc(size);\n" ++
  "}\n" ++
  "\n" ++
  "void* new_literal(size_t size) {\n" ++
      "\treturn malloc(size);\n" ++
  "}\n\n"
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
constPredefs :: CCode
constPredefs =
  "typedef void* gen_closure_clfunc(void*, void*);\n" ++
  "\n" ++
  "typedef struct {\n" ++
      "\tgen_closure_clfunc* clfunc;\n" ++
      "\tchar isLazy;\n" ++
  "} gen_closure;\n" ++
  "\n"
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
mainfn :: CCode
mainfn =
  "int main(void) {\n" ++
      "\treturn (size_t)(main_func());\n" ++
  "}\n"
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
mainfnEmpty :: CCode
mainfnEmpty =
  "int main(void) {\n" ++
      "\treturn 0;\n" ++
  "}\n"
{- FOURMOLU_ENABLE -}

stdDefinitions :: CCode
stdDefinitions =
  Li.intercalate
    "\n"
    ( map
        ( \case
            Left strs -> Li.intercalate "\n" strs
            Right a -> genStdDefinitionCode a
        )
        rawDefs
    )
 where
  rawDefs =
    map
      ( \lib@(name, _, _) ->
          case genStdRawDefinition lib of
            Left strs -> Left strs
            Right (strs, cnt) -> Right (name, strs, cnt)
      )
      standardLibrary

genStdRawDefinition ::
  ( L.Ident
  , a
  , StdCompilerDefinition [Int]
  ) ->
  Either [CCode] ([CCode], Int)
genStdRawDefinition (_, _, ScdSimple f) = Right (code, maximum ns)
 where
  ff :: Int -> Writer [Int] CCode
  ff n = Writer ("std_var_" ++ show n, [n])
  (code, ns) = runWriter $ f ff
genStdRawDefinition (_, _, ScdLiteral strs) = Left strs

genStdDefinitionCode :: (L.Ident, [CCode], Int) -> CCode
genStdDefinitionCode rawDef =
  let (ExprBuildr _ glStatms _, _) =
        runState
          (genStdDefinitionCodeS rawDef)
          (ExprBuildr [] [] 1)
   in unlines glStatms

{- FOURMOLU_DISABLE -}
genStdDefinitionCodeS ::
  (L.Ident, [CCode], Int) ->
  State ExpressionBuilder ()
genStdDefinitionCodeS rawDef@(name, _, _) = do
  stdExpr <- genStdDefinitionCodeStepS 0 rawDef
  addGlobalStatement $
    "void* " ++ show name ++ "_func(void) {\n" ++
        stdExpr ++ "\n" ++
    "}\n"
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
genStdDefinitionCodeStepS ::
  Int ->
  (L.Ident, [CCode], Int) ->
  State ExpressionBuilder CCode
genStdDefinitionCodeStepS step rawDef@(name, cs, n)
  | step >= n = return $
    concatMap (\m -> "\tvoid* std_var_" ++ show m ++ " = " ++ helper m ++ ";\n") [1..n] ++
    concatMap (("\t" ++) . (++ ";\n")) (init cs) ++
    "\treturn " ++ last cs ++ ";"
  | otherwise = do
      nextExpr <- genStdDefinitionCodeStepS (step + 1) rawDef
      clid <- incBuilderIndex
      clid' <- incBuilderIndex
      let closureTyp = show name ++ "_std_closure_" ++ show clid
      addGlobalStatement $
        "typedef struct {\n" ++
            "\tvoid* clfunc;\n" ++
            "\tchar isLazy;\n" ++
            concatMap (\m -> "\tvoid* capture_" ++ show (m + 1) ++ ";\n") [0..step-1] ++
        "} " ++ closureTyp ++ ";\n" ++
        "\n" ++
        "void* " ++ show name ++ "_" ++ show (step + 1) ++ "_clfunc(" ++ closureTyp ++ "* self, void* param){\n" ++
            nextExpr ++ "\n" ++
        "}\n"
      let closureName = "c" ++ show clid'
      return $
        "\t" ++ closureTyp ++ "* " ++ closureName ++ " = new_closure(sizeof(" ++ closureTyp ++ "));\n" ++
        "\t" ++ closureName ++ "->clfunc = " ++ show name ++ "_" ++ show (step + 1) ++ "_clfunc;\n" ++
        "\t" ++ closureName ++ "->isLazy = 0;\n" ++
        concatMap (\m -> "\t" ++ assignHelper clid' (m + 1) ++ "\n") [0..step-1] ++
        "\treturn " ++ closureName ++ ";"
      where
      helper m
        | m >= n = "param"
        | otherwise = "self->capture_" ++ show m
{- FOURMOLU_ENABLE -}
