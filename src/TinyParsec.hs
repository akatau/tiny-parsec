module TinyParsec where

{- |
Module      : TinyParsec
Description : Minimal parsing library using monadic parser combinators in Haskell  
Copyright   : (c) Amr Farid Almorsi, 2023
License     : BSD3
Maintainer : amrfalmorsi@gmail.com
Stability   : experimental
Portability : POSIX

This module provides definitions for the monadic parser comibnators available in TinyParec.
-}


-- | Represents the error produced when parsing fails due to unexpected input.
--
-- The @ParserError@ type encapsulates the expected and actual input values,
-- providing detailed feedback on parsing failures.
data ParserError = ParserError {getExpected :: String, getFound :: String}
    deriving Show

-- | A parser that consumes input and produces either a result or an error.
-- 
-- The @Parser@ type is parameterized over the type of the parsed output.
newtype Parser output = Parser {runParser :: [Char] -> Either (ParserError, [Char]) (output, [Char])}