{-# LANGUAGE InstanceSigs #-}

module Chapter22Reader where

import Control.Applicative
import Data.Char
import Data.Maybe

-- Non-exercices (just composing functions)

hurr :: Num a => a -> a
hurr = (*2)

durr :: Num a => a -> a
durr = (+10)

m :: Num a => a -> a
m = hurr . durr

m' :: Num a => a -> a
m' = fmap hurr durr

m2 :: Num a => a -> a
m2 = (+) <$> hurr <*> durr

m2' :: Num a => a -> a
m2' = liftA2 (+) hurr durr

-- Short Exercise: Warming up (page 873)

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  x' <- cap
  y' <- rev
  return $ (,) x' y'

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= (\x' -> rev >>= (\y' -> return $ (,) x' y'))

-- Exercise: Ask (page 879)

newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

-- Exercise: Reading Comprehension (page 882)

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Ernesto") (DogName "Rocky") (Address "Minerva")

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

getDogR :: Reader Person Dog
getDogR = pure Dog <*> asks dogName <*> asks address

-- Exercise: Reader Monad (page 887)

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

getDogRM :: Reader Person Dog
getDogRM = do
  dn <- asks dogName
  ad <- asks address
  return $ Dog dn ad

-- 22.11 Chapter Exercises (page 890)

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = runReader $ pure (,) <*> asks z' <*> asks z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ and (sequA 6)
  print $ sequA (fromMaybe 0 s')
  print $ bolt (fromMaybe 0 ys)
  print $ bolt (fromMaybe 0 $ z' 5)


sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)
