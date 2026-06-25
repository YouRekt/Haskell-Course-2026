module Spreadsheet.Parser
  ( Parser
  , runParser
  , item
  , zero
  , (<|>)
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
  , addr
  , value
  , chainl1
  , expr
  , parseExpr
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

--   sheet  ::= 'sheet' '{' assign* '}'
--   assign ::= addr '=' expr ';'
--   expr   ::= cmp
--   cmp    ::= sum (('=' | '<' | '>') sum)?
--   sum    ::= term   (('+' | '-') term)*
--   term   ::= factor (('*' | '/') factor)*
--   factor ::= '-' factor | ('SUM'|'AVG') '(' addr ':' addr ')'
--            | addr | value | '(' expr ')'

type Parser a = StateT String [] a

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT

zero :: Parser a
zero = StateT (const [])

item :: Parser Char
item = do
  s <- get
  case s of
    c : cs -> put cs >> pure c
    []     -> zero

infixr 5 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = StateT $ \s ->
  case runStateT p1 s of
    []     -> runStateT p2 s
    parses -> parses

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

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = do
  x  <- p
  xs <- many p
  pure (x : xs)

string :: String -> Parser String
string []       = pure []
string (c : cs) = do
  _ <- char c
  _ <- string cs
  pure (c : cs)

-- Whitespace also swallows '#' line comments.
spaces :: Parser ()
spaces = do
  _ <- many (spaceP <|> comment)
  pure ()

comment :: Parser Char
comment = do
  _ <- char '#'
  _ <- many (sat (/= '\n'))
  pure '#'

token :: Parser a -> Parser a
token p = do
  v <- p
  spaces
  pure v

symbol :: String -> Parser String
symbol cs = token (string cs)

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

addr :: Parser Addr
addr = token $ do
  cs <- many1 (sat isAsciiUpper)
  ds <- many1 digit
  pure (cs, read ds)

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

stringLit :: Parser String
stringLit = token $ do
  _  <- char '"'
  cs <- many (sat (/= '"'))
  _  <- char '"'
  pure cs

-- Handle left-associative operators
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

parseExpr :: String -> Maybe Expr
parseExpr s =
  case runParser (spaces >> expr) s of
    ((e, "") : _) -> Just e
    _             -> Nothing

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
