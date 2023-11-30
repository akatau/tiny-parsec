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

-- | Functor instance for the @Parser@ type.
instance Functor Parser where
    fmap f parserA = Parser (\input ->
                              case runParser parserA input of
                              Right (output, restOfInput) -> Right (f output, restOfInput)
                              Left a -> Left a
                              )

-- | Applicative instance for the @Parser@ type.
instance Applicative Parser where
    pure x = Parser (\input -> Right (x, input))
    parserF <*> parserA = Parser (\input -> 
                                   case runParser parserF input of
                                   Right (f, restOfInput) -> runParser (fmap f parserA) restOfInput
                                   Left a                 -> Left a
                                 )

-- | Monad instance for the @Parser@ type.
instance Monad Parser where
  return x = Parser $ \input -> Right (x, input)
  parserA >>= f = Parser $ \input -> case runParser parserA input of
                                       Right (output, restOfInput) -> runParser (f output) restOfInput
                                       Left a -> Left a

-- | A parser that attempts to consume any single character from the input.
any :: Parser Char
any = Parser $ \input ->
        case input of
            (char:chars) -> Right (char, chars)
            _ -> Left (ParserError "Expected: Char" "Found: EoF", input)

-- | Creates a parser that matches the end-of-file marker.
--
-- This parser expects the input to be empty, producing an error otherwise.
eofParser :: Parser [Char]
eofParser = Parser (\input -> case input of
                    []       -> Right ("", [])
                    (char:_) -> Left (ParserError "Expected: EoF" ("Found: " ++ [char]), input))
