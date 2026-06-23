{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

--
-- ==========================================
--  Tests for SpreadsheetLang.
--
--  Three layers, as the project asks for:
--    * unit tests   — the parser and each
--      kind of evaluation, including the
--      tiny A1 = A1 cycle;
--    * end-to-end    — a sheet we can work
--      out by hand, and a cyclic sheet that
--      must terminate;
--    * property      — invariants checked by
--      QuickCheck over randomly generated
--      expressions and sheets.
-- ==========================================
--

import Data.Either (isRight)
import Data.List (nub)
import qualified Data.Map as Map
import System.Exit (exitFailure)
import Test.QuickCheck

import Spreadsheet

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  Generators (orphan Arbitrary instances)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- A column of one to three upper-case letters; only A-Z, so the address
-- always pretty-prints back to something the parser accepts.
genCol :: Gen String
genCol = do
  n <- choose (1, 3)
  vectorOf n (elements ['A' .. 'Z'])

genRow :: Gen Int
genRow = choose (1, 99)

genAddr :: Gen Addr
genAddr = (,) <$> genCol <*> genRow

-- A string with no quotes/backslashes, so @show@-ing it and parsing it back
-- round-trips (the parser keeps string literals deliberately simple).
genSafeString :: Gen String
genSafeString = do
  n <- choose (0, 6)
  vectorOf n (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " "))

-- A literal value as it could appear on the right of an assignment: a
-- *non-negative* whole number (negatives come from 'Neg'), a boolean or a
-- safe string. Never an 'ErrV' — those are produced by evaluation, never
-- written down.
genValueLit :: Gen Value
genValueLit = oneof
  [ NumV . fromIntegral <$> (choose (0, 1000) :: Gen Int)
  , BoolV <$> arbitrary
  , StrV  <$> genSafeString
  ]

instance Arbitrary Expr where
  arbitrary = sized gen
    where
      gen 0 = oneof [ LitE <$> genValueLit, Ref <$> genAddr ]
      gen n = frequency
        [ (2, LitE  <$> genValueLit)
        , (2, Ref   <$> genAddr)
        , (1, Neg   <$> gen (n - 1))
        , (1, Range <$> elements [SumR, AvgR] <*> genAddr <*> genAddr)
        , (3, BinOp <$> elements [Add, Sub, Mul, Div, Eq, Lt, Gt]
                    <*> gen half <*> gen half)
        ]
        where half = n `div` 2

  -- Shrink towards sub-expressions only, so shrunk values stay valid
  -- (no accidental negative literals or odd addresses).
  shrink (Neg e)        = e : map Neg (shrink e)
  shrink (BinOp op a b) =
    [a, b]
      ++ [ BinOp op a' b | a' <- shrink a ]
      ++ [ BinOp op a b' | b' <- shrink b ]
  shrink _              = []

instance Arbitrary Content where
  arbitrary = oneof [ Lit <$> genValueLit, Form <$> resize 4 arbitrary ]

instance Arbitrary Cell where
  arbitrary = Cell <$> genAddr <*> arbitrary

instance Arbitrary Sheet where
  arbitrary = do
    k     <- choose (0, 8)
    cells <- vectorOf k arbitrary
    pure (Sheet cells)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  Properties
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Pretty-printing an expression and parsing it back recovers it exactly.
prop_expr_roundtrip :: Expr -> Property
prop_expr_roundtrip e = parseExpr (prettyExpr e) === Just e

-- Evaluation is a pure function of the sheet: recomputing with no change
-- yields identical values (and, since QuickCheck forces the result, this
-- also shows evaluation always terminates — even on cyclic sheets).
prop_recompute :: Sheet -> Bool
prop_recompute s = evalSheet s == evalSheet s

-- Every defined cell — and only those — ends up with exactly one value.
prop_every_cell_has_value :: Sheet -> Bool
prop_every_cell_has_value s@(Sheet cells) =
  Map.keys (evalSheet s) == distinctAddrs
  where
    -- the distinct addresses, in the same (sorted) order Map.keys uses
    distinctAddrs = nub (Map.keys (Map.fromList [ (addr c, ()) | c <- cells ]))


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  Small helpers for the unit / e2e checks
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Run a source sheet and read back the value of one cell.
cellVal :: String -> Addr -> Maybe Value
cellVal src a =
  case run src of
    Left _   -> Nothing
    Right cs -> lookup a cs

exampleSrc :: String
exampleSrc = unlines
  [ "sheet {"
  , "  A1 = 10;"
  , "  A2 = 20;"
  , "  A3 = A1 + A2;"
  , "  A4 = SUM(A1:A3);"
  , "  A5 = AVG(A1:A3);"
  , "}"
  ]

cycleSrc :: String
cycleSrc = unlines
  [ "sheet {"
  , "  A1 = A2 + 1;"
  , "  A2 = A1 + 1;"
  , "  B1 = 5;"
  , "  B2 = B1 * 2;"
  , "}"
  ]

unit :: String -> Bool -> IO Bool
unit name ok = do
  putStrLn ((if ok then "[ok]   " else "[FAIL] ") ++ name)
  pure ok

runProp :: String -> Property -> IO Bool
runProp name p = do
  putStrLn ("--- " ++ name ++ " ---")
  isSuccess <$> quickCheckResult p


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  Test runner
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

main :: IO ()
main = do
  putStrLn "Unit and end-to-end checks:"
  units <- sequence
    [ unit "parseExpr respects precedence"
        (parseExpr "1 + 2 * 3"
           == Just (BinOp Add (LitE (NumV 1))
                              (BinOp Mul (LitE (NumV 2)) (LitE (NumV 3)))))
    , unit "parseSheet accepts a valid sheet"
        (isRight (parseSheet "sheet { A1 = 1; }"))
    , unit "parseSheet rejects a malformed cell"
        (not (isRight (parseSheet "sheet { A1 = ; }")))
    , unit "eval: addition"         (cellVal "sheet { A1 = 2 + 3; }" ("A", 1) == Just (NumV 5))
    , unit "eval: multiplication"   (cellVal "sheet { A1 = 4 * 5; }" ("A", 1) == Just (NumV 20))
    , unit "eval: comparison"       (cellVal "sheet { A1 = 3 < 5; }" ("A", 1) == Just (BoolV True))
    , unit "eval: division by zero" (cellVal "sheet { A1 = 1 / 0; }" ("A", 1) == Just (ErrV "div0"))
    , unit "eval: missing reference" (cellVal "sheet { A1 = B9; }"   ("A", 1) == Just (ErrV "#REF"))
    , unit "eval: SUM range"
        (cellVal "sheet { A1 = 1; A2 = 2; A3 = SUM(A1:A2); }" ("A", 3) == Just (NumV 3))
    , unit "cycle: A1 = A1 is ErrV cycle, not a hang"
        (cellVal "sheet { A1 = A1; }" ("A", 1) == Just (ErrV "cycle"))
    , unit "e2e: example sheet A4 = 60"
        (cellVal exampleSrc ("A", 4) == Just (NumV 60))
    , unit "e2e: example sheet A5 = 20"
        (cellVal exampleSrc ("A", 5) == Just (NumV 20))
    , unit "e2e: cyclic sheet terminates, independent cells fine"
        (cellVal cycleSrc ("A", 1) == Just (ErrV "cycle")
           && cellVal cycleSrc ("B", 2) == Just (NumV 10))
    ]

  putStrLn "\nProperties:"
  props <- sequence
    [ runProp "expr pretty/parse round-trip"    (property prop_expr_roundtrip)
    , runProp "recompute is idempotent"         (property prop_recompute)
    , runProp "every defined cell has a value"  (property prop_every_cell_has_value)
    ]

  if and (units ++ props)
    then putStrLn "\nAll tests passed."
    else putStrLn "\nSome tests FAILED." >> exitFailure
