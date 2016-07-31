module Main where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
--main = quickBatch $ applicative (undefined :: List (String, String, Int))
main = quickBatch $ applicative (undefined :: ZipList' (List String, List String, List Int))

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys         = ys
  mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x               = Cons x Nil
  (<*>) Nil _          = Nil
  (<*>) _ Nil          = Nil
  (<*>) (Cons f fs) xs = (fmap f xs) `mappend` (fs <*> xs)

toList :: [a] -> List a
toList = foldr Cons Nil

take' :: Int -> List a -> List a
take' _ Nil         = Nil
take' n _ | n <= 0  = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Eq a => EqProp (List a) where
  xs =-= ys = take' 3000 xs `eq` take' 3000 ys

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap toList arbitrary

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' (fs <*> xs)
