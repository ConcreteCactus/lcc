module SemanticAnalyzer.SemExpression
  ( SemExpression (..),
    Env (..),
    createSemanticExpression,
    createSemanticExpressionS,
    addGlobal,
    startingEnv,
    addDecl,
    findDecl,
  )
where

import Data.Foldable
import Errors
import SemanticAnalyzer.SemType
import SyntacticAnalyzer
import Util

data SemExpression
  = SId Int
  | SRef String
  | SLit Literal
  | SLambda String SemExpression
  | SApplication SemExpression SemExpression
  deriving (Eq)

data Env = Env {globals :: [String], scope :: [String], decls :: [(String, SemType)], globalInfers :: [(String, SemType)]} deriving (Eq, Show)

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

createSemanticExpressionS ::
  SynExpression -> State Env SemExpression
createSemanticExpressionS (Id name) = do
  identM <- findVar name
  case identM of
    Just ident -> return $ SId ident
    Nothing -> do
      globalM <- findGlobal name
      case globalM of
        Just global -> return $ SRef global
        Nothing -> do
          addGlobal name
          return $ SRef name
createSemanticExpressionS (Lambda param expr) = do
  addVar param
  sformula <- createSemanticExpressionS expr
  popVar
  return $ SLambda param sformula
createSemanticExpressionS (Application func arg) = do
  sfunc <- createSemanticExpressionS func
  sarg <- createSemanticExpressionS arg
  return $ SApplication sfunc sarg
createSemanticExpressionS (Lit l) = return $ SLit l

createSemanticExpression :: SynExpression -> SemExpression
createSemanticExpression expr = execState (createSemanticExpressionS expr) startingEnv

startingEnv :: Env
startingEnv = Env [] [] [] []

addGlobal :: String -> State Env ()
addGlobal newGlobal = do
  env <- get
  put $ env {globals = globals env ++ [newGlobal]}

addVar :: String -> State Env ()
addVar newVar = do
  env <- get
  put $ env {scope = newVar : scope env}

popVar :: State Env ()
popVar = do
  env <- get
  put $ env {scope = tail $ scope env}

findVar :: String -> State Env (Maybe Int)
findVar idName = do
  env <- get
  let scopeVars = scope env
  let (foundNameM, count) = foldr (\name (found, n) -> if name == idName then (Just name, 1) else (found, n + 1)) (Nothing, 0) scopeVars
  case foundNameM of
    Nothing -> return Nothing
    Just _ -> return $ Just count

findGlobal :: String -> State Env (Maybe String)
findGlobal idName = do
  env <- get
  let globalVars = globals env
  let foundNameM = find (== idName) globalVars
  return foundNameM

findDecl :: String -> State Env (Maybe SemType)
findDecl idName =
  lookup idName . decls <$> get

addDecl :: String -> SemType -> State Env ()
addDecl name semType = do
  env <- get
  put $ env {decls = (name, semType) : decls env}
