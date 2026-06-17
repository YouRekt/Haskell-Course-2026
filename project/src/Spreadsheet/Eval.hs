module Spreadsheet.Eval
  ( -- * Environments
    Env

    -- * Range helpers
  , colToInt
  , intToCol
  , expandRange

    -- * Evaluating a single expression
  , evalExpr
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Spreadsheet.Types

--
-- ==========================================
--  Evaluating spreadsheet expressions.
--
--  Given an environment that already maps the
--  cells we depend on to their values, an
--  expression evaluates to a single 'Value'.
--  Anything that goes wrong (a missing cell,
--  a division by zero, a type mismatch)
--  becomes an 'ErrV' that propagates outward
--  instead of crashing the sheet.
-- ==========================================
--

-- The values computed for cells so far. Building this map in the right
-- order is the evaluator's job (see "Spreadsheet" / cycle detection).
type Env = Map Addr Value


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 1. Columns and ranges
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Column letters form a bijective base-26 number: "A" = 1, "Z" = 26,
-- "AA" = 27, ... These two functions convert back and forth so we can
-- enumerate the columns between two addresses.
colToInt :: String -> Int
colToInt = foldl step 0
  where
    step acc c = acc * 26 + (fromEnum c - fromEnum 'A' + 1)

intToCol :: Int -> String
intToCol n
  | n <= 0    = ""
  | otherwise = intToCol q ++ [toEnum (fromEnum 'A' + r)]
  where
    (q, r) = (n - 1) `divMod` 26

-- Every address inside the rectangle spanned by two corners, e.g.
-- @expandRange ("A",1) ("B",2) == [("A",1),("A",2),("B",1),("B",2)]@.
-- The corners may be given in any order.
expandRange :: Addr -> Addr -> [Addr]
expandRange (c1, r1) (c2, r2) =
  [ (intToCol c, r)
  | c <- [min ci1 ci2 .. max ci1 ci2]
  , r <- [min r1 r2 .. max r1 r2]
  ]
  where
    ci1 = colToInt c1
    ci2 = colToInt c2


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 2. Evaluating expressions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Evaluate an expression against an environment of already-computed cells.
evalExpr :: Env -> Expr -> Value
evalExpr env = go
  where
    go (LitE v)       = v
    go (Ref a)        = maybe (ErrV "#REF") id (Map.lookup a env)
    go (Neg e)        = negV (go e)
    go (BinOp op a b) = applyOp op (go a) (go b)
    go (Range ro a b) = applyRange ro (rangeValues env a b)

-- The values of the cells in a range that actually exist. Blank cells
-- (addresses with no definition) are simply skipped, as in a real
-- spreadsheet; an error in any cell is kept so it can propagate.
rangeValues :: Env -> Addr -> Addr -> [Value]
rangeValues env a b =
  [ v | addr' <- expandRange a b, Just v <- [Map.lookup addr' env] ]

negV :: Value -> Value
negV (NumV n) = NumV (negate n)
negV (ErrV e) = ErrV e
negV _        = ErrV "type"

-- Binary operators. An 'ErrV' on either side wins, so errors propagate.
applyOp :: Op -> Value -> Value -> Value
applyOp _   (ErrV e) _        = ErrV e
applyOp _   _        (ErrV e) = ErrV e
applyOp Add (NumV x) (NumV y) = NumV (x + y)
applyOp Sub (NumV x) (NumV y) = NumV (x - y)
applyOp Mul (NumV x) (NumV y) = NumV (x * y)
applyOp Div (NumV x) (NumV y) = if y == 0 then ErrV "div0" else NumV (x / y)
applyOp Lt  (NumV x) (NumV y) = BoolV (x < y)
applyOp Gt  (NumV x) (NumV y) = BoolV (x > y)
applyOp Eq  a        b        = eqV a b
applyOp _   _        _        = ErrV "type"

-- Equality is defined within a type; comparing across types is an error.
eqV :: Value -> Value -> Value
eqV (NumV x)  (NumV y)  = BoolV (x == y)
eqV (BoolV x) (BoolV y) = BoolV (x == y)
eqV (StrV x)  (StrV y)  = BoolV (x == y)
eqV _         _         = ErrV "type"

-- Fold a range of values with SUM or AVG. Non-numbers are a type error;
-- an error anywhere in the range propagates; AVG of nothing is undefined.
applyRange :: RangeOp -> [Value] -> Value
applyRange ro vs =
  case numbers vs of
    Left err -> err
    Right ns -> case ro of
      SumR -> NumV (sum ns)
      AvgR -> if null ns then ErrV "div0"
                         else NumV (sum ns / fromIntegral (length ns))

-- Turn a list of values into a list of numbers, or the first problem found.
numbers :: [Value] -> Either Value [Double]
numbers = foldr step (Right [])
  where
    step (NumV n) (Right ns) = Right (n : ns)
    step (ErrV e) _          = Left (ErrV e)
    step _        (Right _)  = Left (ErrV "type")
    step _        acc        = acc
