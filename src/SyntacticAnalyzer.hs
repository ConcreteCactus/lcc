module SyntacticAnalyzer () where

import Lexer

data Expression
  = Lambda
      { param :: String,
        formula :: Expression
      }
  | Identifier String

lambdaExpression :: Parser Expression
lambdaExpression = do
  lambda
  param <- identifier
  dot
  formula <- identifier
  return $ Lambda param $ Identifier formula
