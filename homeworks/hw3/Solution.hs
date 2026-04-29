module Solution () where
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Internal.TH.Lib (safe)
import Data.Text.Array (new)

-- Ex.1

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    dirsFromCurrentPos <- Map.lookup pos maze
    Map.lookup dir dirsFromCurrentPos

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ pos [] = return pos
followPath maze pos (d:ds) = do
    newPos <- move maze pos d
    followPath maze newPos ds

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ pos [] = return [pos]
safePath maze pos (d:ds) = do
    newPos <- move maze pos d
    safePathTail <- safePath maze newPos ds

    return (newPos:safePathTail)

-- Ex.2

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)