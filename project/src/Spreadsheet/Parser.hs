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

    -- * Addresses and literal values
  , addr
  , value

    -- * Expressions
  , chainl1
  , expr
  , parseExpr

    -- * Sheets
  , parseSheet
  ) where

import Control.Monad.State
import Data.Char (isAsciiUpper, isDigit, isSpace)
import Spreadsheet.Types
  ( Addr
  , Cell (Cell)
  , Content (..)
  , Expr (..)
  , Op (..)
  , RangeOp (..)
  , Sheet (..)
  , Value (..)
  )

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

-- Skip whitespace *and* @#@ line comments — anywhere a token may be
-- followed by spaces it may now also be followed by a comment.
spaces :: Parser ()
spaces = do
  _ <- many (spaceP <|> comment)
  pure ()

-- A @#@ comment runs to the end of the line. We treat the whole comment as
-- a single "whitespace" character so 'many' above can keep going; the
-- terminating newline (if any) is then eaten by 'spaceP'.
comment :: Parser Char
comment = do
  _ <- char '#'
  _ <- many (sat (/= '\n'))
  pure '#'

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


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 5. Addresses and literal values
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- A cell address: one or more upper-case column letters followed by a row
-- number, e.g. @A1@, @B12@ or @AA3@.
addr :: Parser Addr
addr = token $ do
  cs <- many1 (sat isAsciiUpper)
  ds <- many1 digit
  pure (cs, read ds)

-- A literal value: a number, a quoted string, or the booleans TRUE/FALSE.
value :: Parser Value
value =
      numLit
  <|> boolLit
  <|> strLit
  where
    numLit  = NumV <$> number

    boolLit =
          (symbol "TRUE"  >> pure (BoolV True))
      <|> (symbol "FALSE" >> pure (BoolV False))

    strLit  = StrV <$> stringLit

-- A double-quoted string literal (no escape sequences — kept simple).
stringLit :: Parser String
stringLit = token $ do
  _  <- char '"'
  cs <- many (sat (/= '"'))
  _  <- char '"'
  pure cs


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 6. The expression grammar
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--   expr   ::= cmp
--   cmp    ::= sum (('=' | '<' | '>') sum)?     -- comparisons yield BoolV
--   sum    ::= term   (('+' | '-') term)*       -- left-associative
--   term   ::= factor (('*' | '/') factor)*     -- left-associative
--   factor ::= '-' factor
--            | ('SUM' | 'AVG') '(' addr ':' addr ')'
--            | addr
--            | value
--            | '(' expr ')'
--

-- Parse one or more @p@ separated by a left-associative binary @op@. Lets
-- us write @100 - 10 - 1@ and get @(100 - 10) - 1 = 89@, not @91@.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x =
      (do
         f <- op
         y <- p
         rest (f x y))
      <|> pure x

expr :: Parser Expr
expr = cmp

cmp :: Parser Expr
cmp = do
  l <- sumE
  (do
     op <- cmpop
     r  <- sumE
     pure (BinOp op l r))
   <|> pure l

sumE, term, factor :: Parser Expr
sumE = term   `chainl1` addop
term = factor `chainl1` mulop
factor =
      neg
  <|> rangeE
  <|> ref
  <|> lit
  <|> paren
  where
    neg = do
      _ <- symbol "-"
      e <- factor
      pure (Neg e)

    ref = Ref <$> addr
    lit = LitE <$> value

    paren = do
      _ <- symbol "("
      e <- expr
      _ <- symbol ")"
      pure e

-- A range operation such as @SUM(A1:A5)@ or @AVG(B2:B9)@.
rangeE :: Parser Expr
rangeE = do
  ro <- rangeop
  _  <- symbol "("
  a  <- addr
  _  <- symbol ":"
  b  <- addr
  _  <- symbol ")"
  pure (Range ro a b)

addop :: Parser (Expr -> Expr -> Expr)
addop =
      (symbol "+" >> pure (BinOp Add))
  <|> (symbol "-" >> pure (BinOp Sub))

mulop :: Parser (Expr -> Expr -> Expr)
mulop =
      (symbol "*" >> pure (BinOp Mul))
  <|> (symbol "/" >> pure (BinOp Div))

cmpop :: Parser Op
cmpop =
      (symbol "=" >> pure Eq)
  <|> (symbol "<" >> pure Lt)
  <|> (symbol ">" >> pure Gt)

rangeop :: Parser RangeOp
rangeop =
      (symbol "SUM" >> pure SumR)
  <|> (symbol "AVG" >> pure AvgR)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 7. Top-level expression parser
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Parse a complete expression, requiring that all the input is consumed.
parseExpr :: String -> Maybe Expr
parseExpr s =
  case runParser (spaces >> expr) s of
    ((e, "") : _) -> Just e
    _             -> Nothing


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 8. The sheet grammar
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--   sheet  ::= 'sheet' '{' assign* '}'
--   assign ::= addr '=' expr ';'
--

-- A single @addr = expr ;@ assignment. A right-hand side that is just a
-- literal becomes 'Lit' content; anything else is a 'Form'ula — that split
-- is what later lets the evaluator tell formula cells apart from data.
assignment :: Parser Cell
assignment = do
  a <- addr
  _ <- symbol "="
  e <- expr
  _ <- symbol ";"
  pure (Cell a (classify e))

classify :: Expr -> Content
classify (LitE v) = Lit v
classify e        = Form e

sheetP :: Parser Sheet
sheetP = do
  _  <- symbol "sheet"
  _  <- symbol "{"
  cs <- many assignment
  _  <- symbol "}"
  pure (Sheet cs)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 9. Top-level sheet parser
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Parse a whole sheet. On failure we report the line of the first input we
-- could not consume — enough to point a beginner at the offending cell.
parseSheet :: String -> Either String Sheet
parseSheet s =
  case runParser (spaces >> sheetP) s of
    ((sh, "")   : _) -> Right sh
    ((_,  rest) : _) -> Left (errorAt rest)
    []               -> Left "syntax error: input is not a valid sheet"
  where
    errorAt rest =
      let consumed = length s - length rest
          line     = 1 + length (filter (== '\n') (take consumed s))
      in "syntax error at line " ++ show line
           ++ ": unexpected " ++ show (takeWhile (/= '\n') rest)
