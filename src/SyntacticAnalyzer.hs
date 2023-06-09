module SyntacticAnalyzer () where

import Lexer

data Expression
  = Lambda
      { param :: String,
        formula :: Expression
      }
  | Identifier String
  deriving (Show)

lambdaExpression :: Parser Expression
lambdaExpression = Lambda <$> (lambda *> identifier <* dot) <*> (Identifier <$> identifier)
