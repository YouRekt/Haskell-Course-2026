{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Data.Either (isRight)
import Data.List (nub)
import qualified Data.Map as Map
import System.Exit (exitFailure)
import Test.QuickCheck

import Spreadsheet

genCol :: Gen String
genCol = do
  n <- choose (1, 3)
  vectorOf n (elements ['A' .. 'Z'])

genRow :: Gen Int
genRow = choose (1, 99)

genAddr :: Gen Addr
genAddr = (,) <$> genCol <*> genRow

genSafeString :: Gen String
genSafeString = do
  n <- choose (0, 6)
  vectorOf n (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " "))

-- Literals as they can appear in source: non-negative numbers (negatives
-- come from Neg), booleans, and quote-free strings, so they round-trip.
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

prop_expr_roundtrip :: Expr -> Property
prop_expr_roundtrip e = parseExpr (prettyExpr e) === Just e

prop_recompute :: Sheet -> Bool
prop_recompute s = evalSheet s == evalSheet s

prop_every_cell_has_value :: Sheet -> Bool
prop_every_cell_has_value s@(Sheet cells) =
  Map.keys (evalSheet s) == distinctAddrs
  where
    distinctAddrs = nub (Map.keys (Map.fromList [ (addr c, ()) | c <- cells ]))

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
