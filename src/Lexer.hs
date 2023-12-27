{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Lexer
  ( VarIdent(..),
    TypIdent,
    TypName(..),
    Literal (..),
    ParserE,
    SourceCode,
    sepBy1,
    sepBy,
    leftAssoc,
    leftAssoc2,
    colon,
    arrow,
    colonEquals,
    backSlash,
    dot,
    exprWhiteSpace,
    stmtWhiteSpace,
    if_,
    then_,
    else_,
    varIdent,
    typIdent,
    typName,
    literal,
    openingBracket,
    closingBracket,
    endOfFile,
    collapseMaybe,
    collapseEither,
    currentPos,
    execParser,
    -- traceParser
  )
where

import Lexer.Internal
