module Solution () where
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.List
import Control.Monad.Trans.Writer
import Text.XHtml (base)

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

-- Ex.5

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving (Show)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)
simplify (Add l r) = do
    l' <- simplify l
    r' <- simplify r
    case (l',r') of
        (Lit 0, e) -> do 
            tell ["Add identity: 0 + e -> e"]
            return e
        (e, Lit 0) -> do
            tell ["Add identity: e + 0 -> e"]
            return e
        (Lit n, Lit m) -> do
            tell ["Constant folding: " ++ show n ++ " + " ++ show m]
            return (Lit (n+m))
        _ -> return (Add l' r')
simplify (Mul l r) = do
    l' <- simplify l
    r' <- simplify r
    case (l',r') of
        (Lit 1, e) -> do
            tell ["Mul identity: 1 * e -> e"]
            return e
        (e, Lit 1) -> do
            tell ["Mul identity: e * 1 -> e"]
            return e
        (Lit 0, Lit n) -> do
            tell ["Zero absorption: 0 * " ++ show n ++ " = 0" ]
            return (Lit 0)
        (Lit n, Lit 0) -> do
            tell ["Zero absorption: " ++ show n ++ " * 0  = 0" ]
            return (Lit 0)
        (Lit n, Lit m) -> do
            tell ["Constant folding: " ++ show n ++ " * " ++ show m]
            return (Lit (n*m))
        _ -> return (Mul l' r')
simplify (Neg n) = do
    m <- simplify n
    case m of
        Neg e -> do
            tell ["Double negation: -(-e) = e"]
            return e
        _ -> return (Neg m)

-- Ex.6

newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

-- (>>=) :: ZipList a -> (a -> ZipList b) -> ZipList b
-- Bind might return ZipLists of different lengths for different elements.
-- Positional pairing requires a fixed "shape", but bind would need to pick
-- a length that depends on runtime values. This breaks associativity:
-- (m >>= f) >>= g \= m >>= (\x -> f x >>= g)
-- Example:
-- m = ZipList [1, 2]
-- Let f and g return different size lists like:
-- f 1 = ZipList [10]
-- f 2 = ZipList [0, 20]
-- g 0 = ZipList []
-- g 10 = ZipList [100]
-- g 20 = ZipList [200, 201]
-- m >>= f = ZipList [10, 20] >>= g = ZipList [100, 201]
-- lambda = \x -> f x >>= g for:
-- x = 1 -> f 1 >>= g = ZipList [10] >>= g = ZipList [100]
-- x = 2 -> f 2 >>= g = ZipList [0, 20] >>= g = ZipList []
--  m >>= lambda = ZipList [100] \= ZipList [100, 201] -> a contradiction!