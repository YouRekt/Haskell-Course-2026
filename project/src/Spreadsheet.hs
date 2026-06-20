module Spreadsheet
  ( -- * Re-exports of the core types
    module Spreadsheet.Types

    -- * Parsing
  , parseExpr
  , parseSheet

    -- * Evaluation
  , evalExpr
  , evalSheet

    -- * One-shot: parse and evaluate
  , run
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map

import Spreadsheet.Types
import Spreadsheet.Parser (parseExpr, parseSheet)
import Spreadsheet.Eval (colToInt, evalExpr, evalSheet)

--
-- ==========================================
--  The public face of SpreadsheetLang: one
--  module that re-exports the core types and
--  the parse/evaluate entry points, plus a
--  'run' that does the whole job at once.
-- ==========================================
--

-- Parse a sheet and evaluate it, returning every cell with its value in a
-- tidy display order (by column, then row), or a parse error message.
run :: String -> Either String [(Addr, Value)]
run src = do
  sheet <- parseSheet src
  pure (sortBy (comparing (displayKey . fst)) (Map.toList (evalSheet sheet)))

-- Sort key that orders A1, A2, ..., B1, ... the way a spreadsheet shows
-- them (columns are compared by their numeric index, not as raw strings,
-- so "Z" comes before "AA").
displayKey :: Addr -> (Int, Int)
displayKey (c, r) = (colToInt c, r)
