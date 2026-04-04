module Solution () where

import Control.Arrow (ArrowChoice (right))
import Data.Foldable (toList)
import qualified Data.List as Ex
import Data.Sequence (Seq)

-- Ex.1

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a) deriving (Show, Eq)

instance Functor Sequence where
  fmap :: (a -> b) -> Sequence a -> Sequence b
  fmap _ Empty = Empty
  fmap f (Single e) = Single (f e)
  fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- Ex.2

instance Foldable Sequence where
  foldMap :: (Monoid m) => (a -> m) -> Sequence a -> m
  foldMap _ Empty = mempty
  foldMap f (Single e) = f e
  foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length

-- Ex.3

instance Semigroup (Sequence a) where
  (<>) :: Sequence a -> Sequence a -> Sequence a
  Empty <> seq = seq
  seq <> Empty = seq
  l <> r = Append l r

instance Monoid (Sequence a) where
  mempty :: Sequence a
  mempty = Empty

-- Ex.4

tailElem :: (Eq a) => a -> Sequence a -> Bool
tailElem target seq = go [seq]
  where
    go [] = False
    go (Empty : xs) = go xs
    go (Single e : xs)
      | e == target = True
      | otherwise = go xs
    go (Append l r : xs) = go (l : r : xs)

-- Ex.5

tailToList :: Sequence a -> [a]
tailToList seq = go [] [seq]
  where
    go acc [] = acc
    go acc (Empty : xs) = go acc xs
    go acc (Single e : xs) = go (e : acc) xs
    go acc (Append l r : xs) = go acc (r : l : xs)

-- Ex.6

data Token = TNum Int | TAdd | TSub | TMul | TDiv deriving (Show, Eq)

tailRPN :: [Token] -> Maybe Int
tailRPN = go []
  where
    go [result] [] = Just result
    go _ [] = Nothing
    go stack (TNum n : ts) = go (n : stack) ts
    go (r : l : stack) (TAdd : ts) = go (l + r : stack) ts
    go (r : l : stack) (TSub : ts) = go (l - r : stack) ts
    go (r : l : stack) (TMul : ts) = go (l * r : stack) ts
    go (0 : l : stack) (TDiv : ts) = Nothing
    go (r : l : stack) (TDiv : ts) = go (l `div` r : stack) ts
    go _ _ = Nothing

-- Ex.7

myReverseFoldr :: [a] -> [a]
myReverseFoldr = foldr (\x acc -> acc <> [x]) []

myReverseFoldl :: [a] -> [a]
myReverseFoldl = foldl (flip (:)) []

myTakeWhileFoldr :: (a -> Bool) -> [a] -> [a]
myTakeWhileFoldr p = foldr go []
  where
    go x acc
      | p x = [x] <> acc
      | otherwise = acc

myDecimalFoldl :: [Int] -> Int
myDecimalFoldl = foldl (\acc x -> 10 * acc + x) 0
