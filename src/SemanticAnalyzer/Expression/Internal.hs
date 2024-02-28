{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SemanticAnalyzer.Expression.Internal where

import Data.Foldable
import qualified Lexer as L
import SemanticAnalyzer.Type
import qualified SyntacticAnalyzer as Y
import Util

data Expression
  = Ident Int
  | Ref L.VarIdent
  | Lit L.Literal
  | Lambda L.VarIdent Expression
  | Application Expression Expression
  | IfThenElse Expression Expression Expression
  deriving (Eq)

data ConvertEnv = ConvertEnv
  { ceGlobals :: [L.VarIdent]
  , ceScope :: [L.VarIdent]
  , ceDecls :: [(L.VarIdent, NormType)]
  }
  deriving (Eq, Show)

instance Show Expression where
  show = showHelper []

showHelper :: [String] -> Expression -> String
showHelper names (Ident identifier) =
  case names !!! identifier of
    Nothing -> "?_" ++ show identifier
    Just name -> name ++ "_" ++ show identifier
showHelper _ (Ref name) = show name
showHelper _ (Lit literal) = show literal
showHelper names (Lambda paramName expr) =
  "λ"
    ++ show paramName
    ++ "."
    ++ showHelper (show paramName : names) expr
showHelper names (Application expr1@(Lambda _ _) expr2@(Application _ _)) =
  "(" ++ showHelper names expr1 ++ ") (" ++ showHelper names expr2 ++ ")"
showHelper names (Application expr1@(Lambda _ _) expr2) =
  "(" ++ showHelper names expr1 ++ ") " ++ showHelper names expr2
showHelper names (Application expr1 expr2@(Application _ _)) =
  showHelper names expr1 ++ " (" ++ showHelper names expr2 ++ ")"
showHelper names (Application expr1 expr2) =
  showHelper names expr1 ++ " " ++ showHelper names expr2
showHelper names (IfThenElse cond expr1 expr2) =
  "if "
    ++ showHelper names cond
    ++ " then "
    ++ showHelper names expr1
    ++ " else "
    ++ showHelper names expr2

convertExpression :: Y.Expression -> Expression
convertExpression expr = 
  execState (convertExpressionS expr) $ ConvertEnv [] [] []

convertExpressionS ::
  Y.Expression -> State ConvertEnv Expression
convertExpressionS (Y.Id name) = do
  identM <- findVar name
  case identM of
    Just ident -> return $ Ident ident
    Nothing -> do
      globalM <- findGlobal name
      case globalM of
        Just global -> return $ Ref global
        Nothing -> do
          addGlobal name
          return $ Ref name
convertExpressionS (Y.Lambda param expr) = do
  addVar param
  sformula <- convertExpressionS expr
  popVar
  return $ Lambda param sformula
convertExpressionS (Y.Application func arg) = do
  sfunc <- convertExpressionS func
  sarg <- convertExpressionS arg
  return $ Application sfunc sarg
convertExpressionS (Y.Lit l) = return $ Lit l
convertExpressionS (Y.IfThenElse cond expr1 expr2) = do
  cond' <- convertExpressionS cond
  expr1' <- convertExpressionS expr1
  expr2' <- convertExpressionS expr2
  return $ IfThenElse cond' expr1' expr2'

addGlobal :: L.VarIdent -> State ConvertEnv ()
addGlobal newGlobal = do
  env <- get
  put $ env{ceGlobals = newGlobal : ceGlobals env}

addVar :: L.VarIdent -> State ConvertEnv ()
addVar newVar = do
  env <- get
  put $ env{ceScope = newVar : ceScope env}

popVar :: State ConvertEnv ()
popVar = do
  env <- get
  put $ env{ceScope = tail $ ceScope env}

findVar :: L.VarIdent -> State ConvertEnv (Maybe Int)
findVar idName = do
  env <- get
  let scopeVars = ceScope env
  let (foundNameM, count) =
        foldr
          ( \name (found, n) ->
              if name == idName
                then (Just name, 1)
                else (found, n + 1)
          )
          (Nothing, 0)
          scopeVars
  case foundNameM of
    Nothing -> return Nothing
    Just _ -> return $ Just count

findGlobal :: L.VarIdent -> State ConvertEnv (Maybe L.VarIdent)
findGlobal idName = do
  env <- get
  let globalVars = ceGlobals env
  let foundNameM = find (== idName) globalVars
  return foundNameM

findDecl :: L.VarIdent -> State ConvertEnv (Maybe NormType)
findDecl idName =
  lookup idName . ceDecls <$> get

addDecl :: L.VarIdent -> NormType -> State ConvertEnv ()
addDecl name typ = do
  env <- get
  put $ env{ceDecls = (name, typ) : ceDecls env}
