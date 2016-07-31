module Chapter17Applicative where

import Control.Applicative
import Data.List (elemIndex)

-- Exercices: Lookups (page 716)
-- 1

added :: Maybe Integer
added = (+3) <$> lookup (3::Integer) (zip [1, 2, 3] [4, 5, 6])

-- 2

y :: Maybe Integer
y = lookup (3::Integer) $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup (2::Integer) $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z

-- 3

x3 :: Maybe Int
x3 = elemIndex (3::Int) [1, 2, 3, 4, 5]

y3 :: Maybe Int
y3 = elemIndex (4::Int) [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' x3 y3

-- 4

xs = [1,2,3]
ys = [4,5,6]

x4 :: Maybe Integer
x4 = lookup (3::Integer) $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> liftA2 (,) x4 y4

-- Identity (page 718)

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

-- Constant (Page 720)

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant f) (Constant a) = Constant (mappend f a)

-- Maybe (Page 721)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
    then Nothing
    else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = Address <$> validateLength 100 s

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = liftA2 Person (mkName n) (mkAddress a)

-- Exercise: Fixer Upper (page 733)
-- 1, 2

fu1 = const <$> Just "Hello" <*> pure "World"

fu2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
