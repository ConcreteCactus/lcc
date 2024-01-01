{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Compiler.Internal where

import Control.Monad
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
  | FunctionRef L.VarIdent
  | CaptureRef Int
  | ParamRef
  | Application Expression Expression
  | IfThenElse Expression Expression Expression
  | Literal Y.Literal
  deriving (Show)

type DeBrujinInd = Int
type MemoryInd = Int

data Closure = Closure
  { clExpression :: Expression
  , clName :: CCode
  , clDepth :: Int
  }
  deriving (Show)

compile :: S.Program -> CCode
compile program =
  includes
    ++ constPredefs
    ++ concatMap ((++ "\n") . predefHelper) (S.progDefs program)
    ++ runtime
    ++ stdDefinitions usedFunctions
    ++ programCode
    ++ if mainfnHelper then mainfn else mainfnEmpty
 where
  usedFunctions =
    foldr
      (\x acc -> getUsedFunctions (mkExpression x) +-+ acc)
      []
      $ S.progDefs program
  programCode = unlines (map genHelper $ S.progDefs program)
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

showExpression :: L.VarIdent -> Expression -> (CCode, CCode)
showExpression gname expr =
  let (ExprBuildr statms gstatms _, expr') =
        runState (showExpressionS gname expr) (ExprBuildr [] [] 1)
   in ( unlines (map ("\t" ++) statms ++ ["\treturn " ++ expr' ++ ";"])
      , unlines gstatms
      )

{- FOURMOLU_DISABLE -}
showExpressionS :: L.VarIdent -> Expression -> State ExpressionBuilder CCode
showExpressionS _ ParamRef = return "param"
showExpressionS _ (Literal (Y.Literal typ lit)) = do
  lid <- incBuilderIndex
  let litName = "l" ++ show lid
  addStatement $ cTypeOf typ ++ "* " ++ litName ++ " = new_literal(sizeof(" ++ cTypeOf typ ++ "));"
  addStatement $ "*" ++ litName ++ " = " ++ show lit ++ ";"
  return litName
showExpressionS _ (CaptureRef n) = return $ "self->captures[" ++ show (n - 2) ++ "]"
showExpressionS _ (FunctionRef func) = do
  return $ show func ++ "_func()"
showExpressionS gname (ClosureExpr closure) = 
    addClosure gname closure
showExpressionS gname (Application expr1 expr2) = do
  expr1' <- showExpressionS gname expr1
  expr2' <- showExpressionS gname expr2
  indx <- incBuilderIndex
  let icl = "icl" ++ show indx
  addStatement $ "closure* " ++ icl ++ " = " ++ expr1' ++ ";"
  return $ icl ++ "->clfunc(" ++ icl ++ ", " ++ expr2' ++ ")"
showExpressionS gname (IfThenElse cond expr1 expr2) = do
  cond' <- showExpressionS gname cond
  indx <- incBuilderIndex
  let ifr = "ifr" ++ show indx
  addStatement $ "void* " ++ ifr ++ ";"
  addStatement $ "if(*(char*)(" ++ cond' ++ ")){"
  expr1' <- showExpressionS gname expr1
  addStatement $ ifr ++ " = " ++ expr1' ++ ";"
  addStatement "} else {"
  expr2' <- showExpressionS gname expr2
  addStatement $ ifr ++ " = " ++ expr2' ++ ";"
  addStatement "}"
  return ifr
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
genFunction :: L.VarIdent -> Expression -> CCode
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
  L.VarIdent ->
  Expression ->
  State ExpressionBuilder CCode
addClosureFunction gname expr = do
  ind <- incBuilderIndex
  expr' <- separateBuilder $ showExpressionS gname expr
  let fnname = show gname ++ "_clfunc_" ++ show ind
  let fncode =
        "void* " ++ fnname ++ "(closure* self, void* param) {\n" ++
            expr' ++ "\n" ++
        "}\n"
  addGlobalStatement fncode
  return fnname
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
addClosure :: L.VarIdent -> Closure -> State ExpressionBuilder CCode
addClosure gname (Closure expr _ depth) = do
  ind <- incBuilderIndex
  let cl = "c" ++ show ind
  addGlobalStatement $ "// depth: " ++ show depth
  clfunc <- addClosureFunction gname expr
  addStatement $
    "closure* " ++ cl ++ " = (closure*)new_closure(" ++ show depth ++ ");"
  addStatement $ cl ++ "->clfunc = " ++ clfunc ++ ";"
  when (depth >= 1) $
      addStatement $ cl ++ "->captures[0] = param;"
  when (depth >= 2) $
    addStatement $ "memcpy(&(" ++ cl ++ "->captures[1]), &(self->captures), sizeof(void*) * " ++ show (depth - 1) ++ ");"
  return $ "c" ++ show ind
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

getUsedFunctions :: Expression -> [L.VarIdent]
getUsedFunctions (ClosureExpr (Closure clExpr _ _)) = getUsedFunctions clExpr
getUsedFunctions (FunctionRef ident) = [ident]
getUsedFunctions (Application expr1 expr2) =
  getUsedFunctions expr1
    +-+ getUsedFunctions expr2
getUsedFunctions (IfThenElse cond expr1 expr2) =
  getUsedFunctions cond
    +-+ getUsedFunctions expr1
    +-+ getUsedFunctions expr2
getUsedFunctions _ = []

mkExpression :: S.Definition -> Expression
mkExpression def =
  let (expr, _) =
        execState
          (mkExpressionS (S.defName def) 0 (S.teExpr (S.defExpr def)))
          1
   in expr

mkExpressionS ::
  L.VarIdent ->
  Int ->
  SE.Expression ->
  State Int (Expression, [Int])
mkExpressionS _ _ (SE.Ident n) | n == 1 = return (ParamRef, [n])
mkExpressionS _ _ (SE.Ident n) = return (CaptureRef n, [n])
mkExpressionS _ _ (SE.Ref n) = return (FunctionRef n, [])
mkExpressionS _ _ (SE.Lit l) = return (Literal l, [])
mkExpressionS ident depth (SE.Lambda _ expr) = do
  (closure, cptrs) <- mkClosure ident depth expr
  return (ClosureExpr closure, cptrs)
mkExpressionS ident depth (SE.Application expr1 expr2) = do
  (expr1', cptrs1) <- mkExpressionS ident depth expr1
  (expr2', cptrs2) <- mkExpressionS ident depth expr2
  return (Application expr1' expr2', Li.sort (cptrs1 +-+ cptrs2))
mkExpressionS ident depth (SE.IfThenElse cond expr1 expr2) = do
  (cond', cptrsc) <- mkExpressionS ident depth cond
  (expr1', cptrs1) <- mkExpressionS ident depth expr1
  (expr2', cptrs2) <- mkExpressionS ident depth expr2
  return
    ( IfThenElse cond' expr1' expr2'
    , Li.sort
        (cptrs1 +-+ cptrs2 +-+ cptrsc)
    )

mkClosure ::
  L.VarIdent ->
  Int ->
  SE.Expression ->
  State Int (Closure, [Int])
mkClosure ident depth expr = do
  (expr', cptrs) <- mkExpressionS ident (depth + 1) expr
  let cptrs' = filter (> 1) cptrs
  clId <- incState
  let closure =
        Closure
          { clExpression = expr'
          , clName = show ident ++ "_c_" ++ show clId
          , clDepth = depth
          }
  return (closure, map (+ (-1)) cptrs')

incState :: State Int Int
incState = do
  state <- get
  put $ state + 1
  return state

compileFull :: SourceCode -> Either CompilerError CCode
compileFull sc = do
  scy <- leftMap mkCompErrLex $ Y.parseProgram sc
  scs <- S.mkProgramFromSyn scy
  return $ compile scs

{- FOURMOLU_DISABLE -}
includes :: CCode
includes = 
    "#include <stdlib.h>\n" ++
    "#include <stdio.h>\n" ++
    "#include <stdint.h>\n" ++
    "#include <string.h>\n" ++
    "\n"
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
runtime :: CCode
runtime =
  "typedef __int128 int128_t;\n" ++
  "typedef unsigned __int128 uint128_t;\n" ++
  "void* new_closure(uint32_t count) {\n" ++
      "\treturn malloc(sizeof(closure) + sizeof(void*) * count);\n" ++
  "}\n" ++
  "\n" ++
  "void* new_literal(size_t size) {\n" ++
      "\treturn malloc(size);\n" ++
  "}\n\n"
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
constPredefs :: CCode
constPredefs =
  "typedef struct closure closure;\n" ++
  "typedef void* closure_clfunc(closure*, void*);\n" ++
  "\n" ++
  "struct closure {\n" ++
      "\tclosure_clfunc* clfunc;\n" ++
      "\tvoid* captures[];\n" ++
  "};\n" ++
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

stdDefinitions :: [L.VarIdent] -> CCode
stdDefinitions usedFunctions =
  Li.intercalate "\n" (map genStdDefinitionCode filteredDefs)
 where
  rawDefs =
    map
      ( \lib@(name, _, _) ->
          let (cs, n) =
                genStdRawDefinition lib
           in (name, cs, n)
      )
      standardLibrary
  filteredDefs = filter (\(n, _, _) -> n `elem` usedFunctions) rawDefs

genStdRawDefinition ::
  ( L.VarIdent
  , a
  , (Int -> Writer [Int] CCode) ->
    Writer [Int] [CCode]
  ) ->
  ([CCode], Int)
genStdRawDefinition (_, _, f) = (code, maximum ns)
 where
  ff :: Int -> Writer [Int] CCode
  ff n = Writer ("std_var_" ++ show n, [n])
  (code, ns) = runWriter $ f ff

genStdDefinitionCode :: (L.VarIdent, [CCode], Int) -> CCode
genStdDefinitionCode rawDef =
  let (ExprBuildr _ glStatms _, _) =
        runState
          (genStdDefinitionCodeS rawDef)
          (ExprBuildr [] [] 1)
   in unlines glStatms

{- FOURMOLU_DISABLE -}
genStdDefinitionCodeS ::
  (L.VarIdent, [CCode], Int) ->
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
  (L.VarIdent, [CCode], Int) ->
  State ExpressionBuilder CCode
genStdDefinitionCodeStepS depth rawDef@(name, cs, n)
  | depth >= n = return $
    concatMap (\m -> "\tvoid* std_var_" ++ show m ++ " = " ++ helper m ++ ";\n") [1..n] ++
    concatMap (("\t" ++) . (++ ";\n")) (init cs) ++
    "\treturn " ++ last cs ++ ";"
  | otherwise = do
      nextExpr <- genStdDefinitionCodeStepS (depth + 1) rawDef
      clid <- incBuilderIndex
      let clfunc = show name ++ "_clfunc_" ++ show (depth + 1)
      addGlobalStatement $
        "void* " ++ clfunc ++ "(closure* self, void* param){\n" ++
            nextExpr ++ "\n" ++
        "}\n"
      let cl = "c" ++ show clid
      return $
        "\tclosure* " ++ cl ++ " = new_closure(" ++ show depth ++ ");\n" ++
        "\t" ++ cl ++ "->clfunc = " ++ clfunc ++ ";\n" ++
        (if depth >= 1
        then "\t" ++ cl ++ "->captures[0] = param;\n"
        else "") ++
        (if depth >= 2
        then "\tmemcpy(&(" ++ cl ++ "->captures[1]), &(self->captures), sizeof(void*) * " ++ show (depth - 1) ++ ");\n"
        else "") ++
        "\treturn " ++ cl ++ ";"
      where
      helper m
        | m >= n = "param"
        | otherwise = "self->captures[" ++ show (n - m - 1) ++ "]"
{- FOURMOLU_ENABLE -}
