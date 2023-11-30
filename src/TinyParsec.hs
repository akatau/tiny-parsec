module TinyParsec where

{- |
Module      : TinyParsec
Description : Minimal parsing library using monadic parser combinators in Haskell  
Copyright   : (c) Amr Farid Almorsi, 2023
License     : BSD3
Maintainer : amrfalmorsi@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the monadic parser comibnators definitinos in TinyParec.
-}

import           Control.Applicative (liftA2)
import           Data.Char
import           Data.Foldable       (for_)
import           Data.Functor
import qualified Data.HashMap.Strict as M
import           Data.List           (intercalate)
import           Prelude             hiding (any)
import           System.Environment
import           Text.Printf
import Data.Map.Strict as Map
import Data.Char


-- | Represents the error produced when parsing fails due to unexpected input.
--
-- The @ParserError@ type encapsulates the expected and actual input values,
-- providing detailed feedback on parsing failures.
data ParserError = ParserError {getExpected :: String, getFound :: String}
    deriving Show
