module Main (main) where

import System.Environment (getArgs)

import Spreadsheet (Addr, Value, prettyAddr, prettyValue, run)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> readFile path >>= report
    []     -> do
      putStrLn "No sheet file given - running the built-in example.\n"
      report exampleSheet
    _      -> putStrLn "usage: project-exe [SHEET-FILE]"

report :: String -> IO ()
report src =
  case run src of
    Left err    -> putStrLn ("Could not parse the sheet:\n  " ++ err)
    Right cells -> mapM_ printCell cells
  where
    printCell :: (Addr, Value) -> IO ()
    printCell (a, v) = putStrLn (prettyAddr a ++ " = " ++ prettyValue v)

exampleSheet :: String
exampleSheet = unlines
  [ "sheet {"
  , "  A1 = 10;"
  , "  A2 = 20;"
  , "  A3 = A1 + A2;"
  , "  A4 = SUM(A1:A3);"
  , "}"
  ]
