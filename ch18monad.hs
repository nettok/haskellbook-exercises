module Chapter18Monad where

import Control.Applicative
import Control.Monad
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Short Exercise: Either Monad (page 784)

data Suma a b = Primero a | Segundo b deriving (Eq, Show)

instance Functor (Suma a) where
  fmap _ (Primero a) = Primero a
  fmap f (Segundo b) = Segundo (f b)

instance Applicative (Suma a) where
  pure = Segundo
  (<*>) (Primero a) _ = Primero a
  (<*>) _ (Primero a) = Primero a
  (<*>) (Segundo f) (Segundo b) = Segundo (f b)

instance Monad (Suma a) where
  return = pure
  (>>=) (Primero a) _ = Primero a
  (>>=) (Segundo b) f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Suma a b) where
  arbitrary = oneof [fmap Primero arbitrary, fmap Segundo arbitrary]

instance (Eq a, Eq b) => EqProp (Suma a b) where
  (=-=) = eq

sumaMonadLaws :: IO ()
sumaMonadLaws = quickBatch $ monad (undefined :: Suma Int (Int, Int, Int))

-- 18.7 Chapter Exercises (page 795)
-- 1

data Nope a = Nope deriving (Eq, Show)

instance Functor Nope where
  fmap _ Nope = Nope

instance Applicative Nope where
  pure _ = Nope
  (<*>) Nope Nope = Nope

instance Monad Nope where
  return = pure
  (>>=) Nope _ = Nope

instance Arbitrary (Nope a) where
  arbitrary = return Nope

instance EqProp (Nope a) where
  (=-=) = eq

nopeMonadLaws :: IO ()
nopeMonadLaws = quickBatch $ monad (undefined :: Nope (Int, Int, Int))

-- 2

data YaSea a b =
    Izq a
  | Der b
  deriving (Eq, Show)

instance Functor (YaSea a) where
  fmap _ (Izq a) = Izq a
  fmap f (Der b) = Der (f b)

instance Applicative (YaSea a) where
  pure = Der
  (<*>) (Izq a) _ = Izq a
  (<*>) _ (Izq a) = Izq a
  (<*>) (Der f) (Der b) = Der (f b)

instance Monad (YaSea a) where
  return = pure
  (>>=) (Izq a) _ = Izq a
  (>>=) (Der b) f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (YaSea a b) where
  arbitrary = oneof [fmap Izq arbitrary, fmap Der arbitrary]

instance (Eq a, Eq b) => EqProp (YaSea a b) where
  (=-=) = eq

yaSeaMonadLaws :: IO ()
yaSeaMonadLaws = quickBatch $ monad (undefined :: YaSea Int (Int, Int, Int))

-- 3

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identityMonadLaws :: IO ()
identityMonadLaws = quickBatch $ monad (undefined :: Identity (Int, Int, Int))

-- 4

data List a = Nil | Cons a (List a) deriving (Eq, Show)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList = foldr Cons Nil

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil xs = xs
  mappend xs Nil = xs
  mappend (Cons x xs) ys = Cons x (xs `mappend` ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) fs (Cons x xs) = fmap ($ x) fs `mappend` (fs <*> xs)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = f x `mappend` (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [
        return Nil
      , do
          a <- arbitrary
          l <- arbitrary
          return $ Cons a l
    ]

instance Eq a => EqProp (List a) where
  (=-=) xs ys = take' 1000 xs `eq` take' 1000 ys

listLaws :: IO ()
listLaws = do
  putStrLn "List laws"
  quickBatch $ monoid (undefined :: List (Int, Int, Int))
  quickBatch $ functor (undefined :: List (Int, Int, Int))
  quickBatch $ applicative (undefined :: List (Int, Int, Int))
  quickBatch $ monad (undefined :: List (Int, Int, Int))

----

j :: Monad m => m (m a) -> m a
j mma = mma >>= id  -- join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= (\a -> fmap (f a) mb)

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _     = return []
meh (x:xs) f = liftM2 (++) (pure <$> f x) (meh xs f)

flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id
