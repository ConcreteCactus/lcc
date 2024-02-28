{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module SyntacticAnalyzer (
        Expression(..),
        Type(..),
        ProgramPart(..),
        Program,
        parseProgram
    )
where

import SyntacticAnalyzer.Internal
