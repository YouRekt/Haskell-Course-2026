module Spreadsheet
  ( module Spreadsheet.Types
  , parseExpr
  , parseSheet
  , evalExpr
  , evalSheet
  , run
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map

import Spreadsheet.Types
import Spreadsheet.Parser (parseExpr, parseSheet)
import Spreadsheet.Eval (colToInt, evalExpr, evalSheet)

run :: String -> Either String [(Addr, Value)]
run src = do
  sheet <- parseSheet src
  pure (sortBy (comparing (displayKey . fst)) (Map.toList (evalSheet sheet)))

displayKey :: Addr -> (Int, Int)
displayKey (c, r) = (colToInt c, r)
