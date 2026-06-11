module Spreadsheet.Parser
  ( -- * The parser type
    Parser
  , runParser

    -- * Primitives
  , item
  , zero
  , (<|>)

    -- * Building blocks
  , sat
  , char
  , digit
  , many
  , many1
  , string
  , spaces
  , token
  , symbol
  , number
  ) where

import Control.Monad.State
import Data.Char (isDigit, isSpace)

--
-- ==========================================
--  The SpreadsheetLang parser, built on the
--  Hutton/Meijer monadic parser from Lecture 7.
--
--  This module starts with the *generic*
--  machinery — the parser type, the three
--  primitives and the small combinators that
--  every grammar is made of. The spreadsheet
--  grammar itself is added on top of these.
-- ==========================================
--

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 1. The Parser type (StateT String [])
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- A parser threads the remaining input (the 'String' state) and uses the
-- list monad to express "this many ways to parse". We get 'Functor',
-- 'Applicative' and 'Monad' for free from the transformer, so we can use
-- do-notation straight away.
type Parser a = StateT String [] a

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 2. Primitives
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- The parser that always fails (no results at all).
zero :: Parser a
zero = StateT (const [])

-- Consume and return one character, failing on empty input.
item :: Parser Char
item = do
  s <- get
  case s of
    c : cs -> put cs >> pure c
    []     -> zero


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 3. Choice (deterministic — first success wins)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- We define our own choice rather than relying on 'Alternative', so it is
-- clear what it does: run the first parser, and only if it produces no
-- results at all fall back to the second.
infixr 5 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = StateT $ \s ->
  case runStateT p1 s of
    []     -> runStateT p2 s
    parses -> parses


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 4. Building blocks
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- One character satisfying a predicate.
sat :: (Char -> Bool) -> Parser Char
sat predicate = do
  c <- item
  if predicate c then pure c else zero

char :: Char -> Parser Char
char c = sat (== c)

digit :: Parser Char
digit = sat isDigit

spaceP :: Parser Char
spaceP = sat isSpace

-- Zero or more / one or more repetitions of a parser.
many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = do
  x  <- p
  xs <- many p
  pure (x : xs)

-- Match a fixed string.
string :: String -> Parser String
string []       = pure []
string (c : cs) = do
  _ <- char c
  _ <- string cs
  pure (c : cs)

-- Skip run-of-the-mill whitespace.
spaces :: Parser ()
spaces = do
  _ <- many spaceP
  pure ()

-- Run a parser and then swallow any trailing whitespace, so the next
-- parser starts on real input. Combinators built from 'token' let us be
-- relaxed about spacing in the grammar.
token :: Parser a -> Parser a
token p = do
  v <- p
  spaces
  pure v

symbol :: String -> Parser String
symbol cs = token (string cs)

-- A (non-negative) number: one or more digits, optionally followed by a
-- decimal part. Negation is handled in the grammar, not here.
number :: Parser Double
number = token $ do
  whole <- many1 digit
  frac  <- decimals <|> pure ""
  pure (read (whole ++ frac))
  where
    decimals = do
      dot <- char '.'
      ds  <- many1 digit
      pure (dot : ds)
