module Chapter20Foldable where

import Data.Foldable
import Data.Monoid

-- Exercises: Library Functions (page 842)

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldl (+) 0

prod' :: (Foldable t, Num a) => t a -> a
prod' = foldl (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = foldl (\acc x -> acc ||  x == e) False

min' :: (Foldable t, Ord a) => t a -> Maybe a
min' ts | null ts = Nothing
min' ts = Just $ foldl1 min ts

max' :: (Foldable t, Ord a) => t a -> Maybe a
max' ts | null ts = Nothing
max' ts = Just $ foldl1 max ts

null' :: (Foldable t) => t a -> Bool
null' = foldl (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldl (\acc _ -> acc + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap' id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x <> acc) mempty


-- 20.6 Chapter Exercises: Write Foldable instances for the following datatypes
-- 1

data Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr _ z (Constant _) = z

-- 2

data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z

-- 3

data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

-- 4

data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr f z (Three' _ b1 b2) = f b2 $ f b1 z

-- 5

data Four a b = Four a b b b deriving (Eq, Show)

instance Foldable (Four a) where
  foldr f z (Four _ b1 b2 b3) = f b3 $ f b2 $ f b1 z


-- Implement filterF using foldMap

filter' :: (Foldable t, Monoid m) => (m -> Bool) -> t m -> m
filter' p = foldl (\acc x -> if p x then acc <> x else acc) mempty

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p = foldMap (\x -> if p x then pure x else mempty)
