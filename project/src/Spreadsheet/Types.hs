module Spreadsheet.Types
  ( Addr
  , Value (..)
  , Op (..)
  , RangeOp (..)
  , Expr (..)
  , Content (..)
  , Cell (..)
  , Sheet (..)
  , prettyAddr
  , prettyValue
  , prettyExpr
  , showNum
  ) where

type Addr = (String, Int)

data Value
  = NumV  Double
  | BoolV Bool
  | StrV  String
  | ErrV  String
  deriving (Eq, Show)

data Op = Add | Sub | Mul | Div | Eq | Lt | Gt
  deriving (Eq, Show)

data RangeOp = SumR | AvgR
  deriving (Eq, Show)

data Expr
  = Ref   Addr
  | LitE  Value
  | Neg   Expr
  | BinOp Op Expr Expr
  | Range RangeOp Addr Addr
  deriving (Eq, Show)

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

prettyAddr :: Addr -> String
prettyAddr (c, r) = c ++ show r

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
