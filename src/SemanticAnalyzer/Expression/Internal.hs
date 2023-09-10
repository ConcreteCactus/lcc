module SemanticAnalyzer.Expression.Internal where

import Data.Foldable
import qualified Lexer as L
import SemanticAnalyzer.Type
import qualified SyntacticAnalyzer as Y
import Util

data Expression
  = Ident Int
  | Ref L.Ident
  | Lit Y.Literal
  | Lambda L.Ident Expression
  | Application Expression Expression
  deriving (Eq)

data ConvertEnv = ConvertEnv {globals :: [L.Ident], scope :: [L.Ident], decls :: [(L.Ident, NormType)], globalInfers :: [(L.Ident, NormType)]} deriving (Eq, Show)

instance Show Expression where
  show = showHelper []

showHelper :: [String] -> Expression -> String
showHelper names (Ident identifier) =
  case names !!! identifier of
    Nothing -> "?_" ++ show identifier
    Just name -> name ++ "_" ++ show identifier
showHelper _ (Ref name) = L.unIdent name
showHelper _ (Lit literal) = show literal
showHelper names (Lambda paramName expr) =
  "λ"
    ++ L.unIdent paramName
    ++ "."
    ++ showHelper (L.unIdent paramName : names) expr
showHelper names (Application expr1@(Lambda _ _) expr2@(Application _ _)) =
  "(" ++ showHelper names expr1 ++ ") (" ++ showHelper names expr2 ++ ")"
showHelper names (Application expr1@(Lambda _ _) expr2) =
  "(" ++ showHelper names expr1 ++ ") " ++ showHelper names expr2
showHelper names (Application expr1 expr2@(Application _ _)) =
  showHelper names expr1 ++ " (" ++ showHelper names expr2 ++ ")"
showHelper names (Application expr1 expr2) =
  showHelper names expr1 ++ " " ++ showHelper names expr2

convertExpression :: Y.Expression -> Expression
convertExpression expr = execState (convertExpressionS expr) $ ConvertEnv [] [] [] []

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

addGlobal :: L.Ident -> State ConvertEnv ()
addGlobal newGlobal = do
  env <- get
  put $ env {globals = globals env ++ [newGlobal]}

addVar :: L.Ident -> State ConvertEnv ()
addVar newVar = do
  env <- get
  put $ env {scope = newVar : scope env}

popVar :: State ConvertEnv ()
popVar = do
  env <- get
  put $ env {scope = tail $ scope env}

findVar :: L.Ident -> State ConvertEnv (Maybe Int)
findVar idName = do
  env <- get
  let scopeVars = scope env
  let (foundNameM, count) = foldr (\name (found, n) -> if name == idName then (Just name, 1) else (found, n + 1)) (Nothing, 0) scopeVars
  case foundNameM of
    Nothing -> return Nothing
    Just _ -> return $ Just count

findGlobal :: L.Ident -> State ConvertEnv (Maybe L.Ident)
findGlobal idName = do
  env <- get
  let globalVars = globals env
  let foundNameM = find (== idName) globalVars
  return foundNameM

findDecl :: L.Ident -> State ConvertEnv (Maybe NormType)
findDecl idName =
  lookup idName . decls <$> get

addDecl :: L.Ident -> NormType -> State ConvertEnv ()
addDecl name typ = do
  env <- get
  put $ env {decls = (name, typ) : decls env}
