module Main where

import Control.Applicative
import Data.Monoid hiding (First)
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


main :: IO ()
main = do
  quickBatch $ applicative (undefined :: Suma String (String, String, Int))
  quickBatch $ applicative (undefined :: Validation [String] (String, String, Int))


-- Exercise: Variatons on Either (page 750)

data Suma a b = First a | Second b deriving (Eq, Show)

data Validation e a = Error e | Success a deriving (Eq, Show)

instance Functor (Suma a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Suma a) where
  pure = Second
  (<*>) (First a) _ = First a
  (<*>) _ (First a) = First a
  (<*>) (Second f) (Second b) = Second (f b)

instance Functor (Validation e) where
  fmap _ (Error e) = Error e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Error e1) (Error e2) = Error (e1 `mappend` e2)
  (<*>) (Error e) _ = Error e
  (<*>) _ (Error e) = Error e
  (<*>) (Success f) (Success a) = Success (f a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Suma a b) where
  arbitrary = oneof [fmap First arbitrary, fmap Second arbitrary]

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [fmap Error arbitrary, fmap Success arbitrary]

instance (Eq a, Eq b) => EqProp (Suma a b) where
  (=-=) = eq

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

-- 17.9 Chapter Excercises
-- 1

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) =  Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) = fmap f

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identityLaws :: IO ()
identityLaws = quickBatch $ applicative (undefined :: Identity (String, String, Int))

-- 2

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f1 f2) (Pair a1 a2) = Pair (f1 a1) (f2 a2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

pairLaws :: IO ()
pairLaws = quickBatch $ applicative (undefined :: Pair (String, String, Int))


-- 3

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two a1 fb) (Two a2 b) = Two (a1 `mappend` a2) (fb b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = fmap Sum arbitrary

twoLaws :: IO ()
twoLaws = quickBatch $ applicative (undefined :: Two (String, String, Sum Int) (String, String, Sum Int))

-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (\a b c -> (a,b,c))
