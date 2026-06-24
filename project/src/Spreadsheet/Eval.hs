module Spreadsheet.Eval
  ( Env
  , colToInt
  , intToCol
  , expandRange
  , evalExpr
  , deps
  , contentDeps
  , dependencies
  , topoOrder
  , evalSheet
  ) where

import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Spreadsheet.Types

type Env = Map Addr Value

-- Column letters are a bijective base-26 number: "A" = 1, "Z" = 26, "AA" = 27.
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

expandRange :: Addr -> Addr -> [Addr]
expandRange (c1, r1) (c2, r2) =
  [ (intToCol c, r)
  | c <- [min ci1 ci2 .. max ci1 ci2]
  , r <- [min r1 r2 .. max r1 r2]
  ]
  where
    ci1 = colToInt c1
    ci2 = colToInt c2

evalExpr :: Env -> Expr -> Value
evalExpr env = go
  where
    go (LitE v)       = v
    go (Ref a)        = maybe (ErrV "#REF") id (Map.lookup a env)
    go (Neg e)        = negV (go e)
    go (BinOp op a b) = applyOp op (go a) (go b)
    go (Range ro a b) = applyRange ro (rangeValues env a b)

rangeValues :: Env -> Addr -> Addr -> [Value]
rangeValues env a b =
  [ v | addr' <- expandRange a b, Just v <- [Map.lookup addr' env] ]

negV :: Value -> Value
negV (NumV n) = NumV (negate n)
negV (ErrV e) = ErrV e
negV _        = ErrV "type"

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

eqV :: Value -> Value -> Value
eqV (NumV x)  (NumV y)  = BoolV (x == y)
eqV (BoolV x) (BoolV y) = BoolV (x == y)
eqV (StrV x)  (StrV y)  = BoolV (x == y)
eqV _         _         = ErrV "type"

applyRange :: RangeOp -> [Value] -> Value
applyRange ro vs =
  case numbers vs of
    Left err -> err
    Right ns -> case ro of
      SumR -> NumV (sum ns)
      AvgR -> if null ns then ErrV "div0"
                         else NumV (sum ns / fromIntegral (length ns))

numbers :: [Value] -> Either Value [Double]
numbers = foldr step (Right [])
  where
    step (NumV n) (Right ns) = Right (n : ns)
    step (ErrV e) _          = Left (ErrV e)
    step _        (Right _)  = Left (ErrV "type")
    step _        acc        = acc

deps :: Expr -> [Addr]
deps (Ref a)       = [a]
deps (LitE _)      = []
deps (Neg e)       = deps e
deps (BinOp _ a b) = deps a ++ deps b
deps (Range _ a b) = expandRange a b

contentDeps :: Content -> [Addr]
contentDeps (Lit _)  = []
contentDeps (Form e) = deps e

dependencies :: Sheet -> Map Addr [Addr]
dependencies (Sheet cells) =
  Map.fromList [ (addr c, contentDeps (content c)) | c <- cells ]

-- Kahn's algorithm: repeatedly place the cells whose dependencies are
-- already placed. Whatever is left when nothing is ready is on a cycle.
-- Returns (evaluation order, cells stuck in a cycle).
topoOrder :: Map Addr [Addr] -> ([Addr], [Addr])
topoOrder depMap = loop [] (Map.keys depMap)
  where
    defined    = Set.fromList (Map.keys depMap)
    realDeps a = filter (`Set.member` defined) (Map.findWithDefault [] a depMap)

    loop ordered remaining =
      let placed            = Set.fromList ordered
          isReady a         = all (`Set.member` placed) (realDeps a)
          (ready, notReady) = partition isReady remaining
      in if null ready
           then (ordered, remaining)
           else loop (ordered ++ ready) notReady

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
