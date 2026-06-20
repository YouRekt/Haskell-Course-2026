module Spreadsheet.Eval
  ( -- * Environments
    Env

    -- * Range helpers
  , colToInt
  , intToCol
  , expandRange

    -- * Evaluating a single expression
  , evalExpr

    -- * Dependency graph
  , deps
  , contentDeps
  , dependencies

    -- * Topological evaluation
  , topoOrder
  , evalSheet
  ) where

import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

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


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 3. The dependency graph
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- The cells an expression refers to *directly*, with ranges expanded to
-- every address they cover. This is the heart of the dependency graph:
-- before a cell can be evaluated, all of these must be known.
deps :: Expr -> [Addr]
deps (Ref a)       = [a]
deps (LitE _)      = []
deps (Neg e)       = deps e
deps (BinOp _ a b) = deps a ++ deps b
deps (Range _ a b) = expandRange a b

-- A literal cell depends on nothing; a formula cell depends on its refs.
contentDeps :: Content -> [Addr]
contentDeps (Lit _)  = []
contentDeps (Form e) = deps e

-- Map every defined cell to the list of cells it depends on. (A reference
-- to an undefined cell stays in the list; the evaluator turns it into a
-- '#REF' value, it just never gets its own node here.)
dependencies :: Sheet -> Map Addr [Addr]
dependencies (Sheet cells) =
  Map.fromList [ (addr c, contentDeps (content c)) | c <- cells ]


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 4. Topological order and cycle detection
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Order the cells so every cell comes after the cells it depends on, using
-- Kahn's algorithm: repeatedly take the cells whose dependencies are all
-- already placed. If at some point nothing is ready but cells remain, those
-- cells are on (or downstream of) a cycle. We return both lists:
-- @(evaluation order, cells stuck in a cycle)@. Dependencies on undefined
-- cells are ignored here — they cannot create a cycle, and the evaluator
-- reports them as '#REF'.
topoOrder :: Map Addr [Addr] -> ([Addr], [Addr])
topoOrder depMap = loop [] (Map.keys depMap)
  where
    defined    = Set.fromList (Map.keys depMap)
    realDeps a = filter (`Set.member` defined) (Map.findWithDefault [] a depMap)

    loop ordered remaining =
      let placed             = Set.fromList ordered
          isReady a          = all (`Set.member` placed) (realDeps a)
          (ready, notReady)  = partition isReady remaining
      in if null ready
           then (ordered, remaining)           -- stuck: the rest are cyclic
           else loop (ordered ++ ready) notReady


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 5. Evaluating a whole sheet
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Evaluate every cell of a sheet to a value. Cells on a cycle become
-- @ErrV "cycle"@; all other cells are evaluated in dependency order, so by
-- the time a formula runs the values it refers to are already in the
-- environment.
evalSheet :: Sheet -> Map Addr Value
evalSheet sheet@(Sheet cells) =
  foldl step cyclesFlagged ordered
  where
    (ordered, cyclic) = topoOrder (dependencies sheet)
    contentOf         = Map.fromList [ (addr c, content c) | c <- cells ]
    cyclesFlagged     = Map.fromList [ (a, ErrV "cycle") | a <- cyclic ]

    step env a =
      case Map.lookup a contentOf of
        Just (Lit v)  -> Map.insert a v env
        Just (Form e) -> Map.insert a (evalExpr env e) env
        Nothing       -> env
