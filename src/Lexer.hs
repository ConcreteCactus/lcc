{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Lexer
  ( Ident (..),
    Parser (..),
    word,
    identifier,
    integer,
    whiteSpace,
    whiteSpaceO,
    dot,
    lambda,
    openParen,
    closeParen,
    capIdentifier,
    arrow,
    colon,
    colonEquals,
    sepBy,
    sepBy1,
    endOfLine,
    statement,
    eof,
    getPos,
    unIdent,
    mapWithError,
  )
where

import Lexer.Internal
