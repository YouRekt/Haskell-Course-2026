module Solution () where
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.List

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

-- Ex.3

type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    arrangement <- permutations guests
    let pairs = zip arrangement (tail arrangement ++ [head arrangement])
    guard $ all noConflict pairs
    return arrangement
    where
        noConflict (a, b) = (a,b) `notElem` conflicts && (b,a) `notElem` conflicts

-- Ex.4

data Result a = Failure String | Success a [String] deriving (Show)

instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Success a warnings) = Success (f a) warnings

instance Applicative Result where
    pure a = Success a []
    Failure msg <*> _ = Failure msg
    Success _ _ <*> Failure msg = Failure msg
    Success f warnings' <*> Success a warnings = Success (f a) (warnings' ++ warnings)

instance Monad Result where
    Failure msg >>= _ = Failure msg
    Success a warnings >>= f = case f a of
        Failure msg -> Failure msg
        Success b warnings' -> Success b (warnings ++ warnings')

warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

validateAge :: Int -> Result Int
validateAge age
    | age < 0 = failure "Age is negative"
    | age > 150 = do
        warn "Age is over 150"
        return age
    | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges ages = do
    mapM validateAge ages