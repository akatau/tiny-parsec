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

-- Useful for creating parsers that fail with specific error messages.
errorParser :: String -> String -> Parser a
errorParser expected found = Parser $ \input -> Left (ParserError expected found, input)


-- | Parses the next character if it satisfies the given predicate.
--
-- Returns the character if it passes the test; otherwise, it reports an error.
satisfyParser :: (Char -> Bool) -> Parser Char
satisfyParser predicate = Parser $ \input ->
                                  case runParser any input of
                                       Right (char, restOfInput) -> if predicate char then Right (char, restOfInput) 
                                                       else Left (ParserError "Expected: Char of certain property." ("Found: " ++[char]), input) 
                                       Left a -> Left a

-- | Tries to apply the first parser, but if it fails, tries the second one instead.
try :: Parser a -> Parser a
try parserA= Parser $ \input -> 
                       case runParser parserA input of
                            Right a -> Right a
                            Left (error, output) -> Left (error, input)

-- | Combines two parsers such that the first one is tried first. If it succeeds, its result is returned;
--   otherwise, the second parser is attempted.
(<|>) :: Parser a -> Parser a -> Parser a
parser1 <|> parser2 = Parser $ \input ->
                                case runParser parser1 input of
                                     Right a -> Right a
                                     Left (error, input) ->  runParser parser2 input 

-- | Chooses the first parser from a list that successfully parses the input.
chooseParser :: [Parser a] -> Parser a 
chooseParser [parser] = parser
chooseParser (firstParser:rest) = firstParser <|> chooseParser rest

-- | Provides a choice among several parsers, returning an error message if none succeed.
choice :: String -> [Parser a] -> Parser a
choice description parsers = Prelude.foldr (<|>) (errorParser description "No match") parsers

-- | Similar to @choice@, but ensures that the list of parsers includes a default failure handler.
choice' :: String -> [Parser a] -> Parser a
choice' description parsers = chooseParser $ parsers ++ [errorParser description "No match."]
