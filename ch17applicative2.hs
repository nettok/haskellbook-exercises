module Chapter17Applicative2 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercise: Variatons on Either (page 750)

data Sum a b = First a | Second b deriving (Eq, Show)

data Validation e a = Error e | Success a deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
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

-- TODO: checkes

-- 17.9 Chapter Excercises
-- TODO NEXT
