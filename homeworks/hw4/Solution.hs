module Solution () where

-- Ex.1

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader a) = Reader (f . a)

instance Applicative (Reader r) where
  pure x = Reader (const x)
  liftA2 f (Reader a) (Reader b) = Reader (\r -> f (a r) (b r))

instance Monad (Reader r) where
  Reader a >>= f = Reader (\r -> runReader (f (a r)) r)