module Spreadsheet.Types
  ( -- * Cell addresses
    Addr

    -- * Values
  , Value (..)

    -- * Operators
  , Op (..)
  , RangeOp (..)

    -- * Expressions and cells
  , Expr (..)
  , Content (..)
  , Cell (..)
  , Sheet (..)

    -- * Pretty-printing
  , prettyAddr
  , prettyValue
  , prettyExpr
  , showNum
  ) where

--
-- ==========================================
--  SpreadsheetLang — the core data types.
--
--  A sheet is a list of cells; every cell
--  lives at an address and either holds a
--  literal value or a formula referring to
--  other cells. Everything else in the
--  project (parser, evaluator) is built on
--  top of these few types.
-- ==========================================
--

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 1. Addresses
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- An address is a column (the letters, e.g. "A" or "AA") together with a
-- row number, e.g. ("A", 1) for the cell @A1@. A plain tuple is enough,
-- and it gives us 'Eq' and 'Ord' for free so we can use addresses as the
-- keys of a 'Data.Map.Map'.
type Addr = (String, Int)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 2. Values
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- The result of evaluating a cell. 'ErrV' carries a short message and is
-- how the evaluator reports problems (division by zero, a cycle, a
-- reference to a missing cell, ...) without crashing the whole sheet.
data Value
  = NumV  Double
  | BoolV Bool
  | StrV  String
  | ErrV  String
  deriving (Eq, Show)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 3. Operators
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Binary operators. The first four are arithmetic; the last three are
-- comparisons and produce a 'BoolV'.
data Op = Add | Sub | Mul | Div | Eq | Lt | Gt
  deriving (Eq, Show)

-- Operators that fold a rectangular range of cells into one value.
data RangeOp = SumR | AvgR
  deriving (Eq, Show)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 4. Expressions, cells and the sheet
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- The little expression language that appears on the right of a cell
-- assignment. A literal 'LitE' always holds a non-negative number (or a
-- bool/string); negation is written explicitly with 'Neg', exactly as the
-- parser produces it.
data Expr
  = Ref   Addr
  | LitE  Value
  | Neg   Expr
  | BinOp Op Expr Expr
  | Range RangeOp Addr Addr   -- e.g. SUM(A1:A5)
  deriving (Eq, Show)

-- A cell holds either a bare literal or a formula. Keeping the two apart
-- mirrors the surface syntax (@A1 = 10@ versus @A3 = A1 + A2@) and makes
-- it obvious which cells can ever take part in the dependency graph.
data Content
  = Lit  Value
  | Form Expr
  deriving (Eq, Show)

data Cell = Cell
  { addr    :: Addr
  , content :: Content
  }
  deriving (Eq, Show)

newtype Sheet = Sheet [Cell]
  deriving (Eq, Show)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 5. Pretty-printing
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Render an address back to its surface form: ("A", 1) -> "A1".
prettyAddr :: Addr -> String
prettyAddr (c, r) = c ++ show r

-- A whole number like 30.0 reads better as "30", while 2.5 stays "2.5".
showNum :: Double -> String
showNum x
  | x == fromInteger r = show r
  | otherwise          = show x
  where
    r = round x :: Integer

prettyValue :: Value -> String
prettyValue (NumV n)   = showNum n
prettyValue (BoolV b)  = if b then "TRUE" else "FALSE"
prettyValue (StrV s)   = show s
prettyValue (ErrV msg) = "#ERR(" ++ msg ++ ")"

-- Fully parenthesised so that parsing the result always recovers the very
-- same expression — a convenient target for the round-trip property tests.
prettyExpr :: Expr -> String
prettyExpr (Ref a)        = prettyAddr a
prettyExpr (LitE v)       = prettyValue v
prettyExpr (Neg e)        = "(-" ++ prettyExpr e ++ ")"
prettyExpr (BinOp op a b) =
  "(" ++ prettyExpr a ++ " " ++ prettyOp op ++ " " ++ prettyExpr b ++ ")"
prettyExpr (Range ro a b) =
  prettyRangeOp ro ++ "(" ++ prettyAddr a ++ ":" ++ prettyAddr b ++ ")"

prettyOp :: Op -> String
prettyOp Add = "+"
prettyOp Sub = "-"
prettyOp Mul = "*"
prettyOp Div = "/"
prettyOp Eq  = "="
prettyOp Lt  = "<"
prettyOp Gt  = ">"

prettyRangeOp :: RangeOp -> String
prettyRangeOp SumR = "SUM"
prettyRangeOp AvgR = "AVG"
