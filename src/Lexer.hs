{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Lexer
  ( Ident (..),
    Parser (..),
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
  )
where

import Lexer.Internal
