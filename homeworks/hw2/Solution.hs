module Solution () where

import Data.Foldable (Foldable (toList))

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a) deriving (Show, Eq)

instance Functor Sequence where
    fmap :: (a -> b) -> Sequence a -> Sequence b
    fmap _ Empty = Empty
    fmap f (Single e) = Single (f e)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

instance Foldable Sequence where
    foldMap :: (Monoid m) => (a -> m) -> Sequence a -> m
    foldMap _ Empty = mempty
    foldMap f (Single e) = f e
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length
