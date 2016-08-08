module Chapter21Traversable where

import Data.Foldable

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 21.12 Chapter Exercises (page 863)
-- Traversable instances + QuickCheck

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

identityCheck :: IO ()
identityCheck = quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))

--

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ z (Constant _) = z

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

constantCheck :: IO ()
constantCheck = quickBatch $ traversable (undefined :: Constant Int (Int, Int, [Int]))

--

data Talvez a =
    No
  | Si a
  deriving (Eq, Show)

instance Functor Talvez where
  fmap _ No     = No
  fmap f (Si a) = Si (f a)

instance Foldable Talvez where
  foldr _ z No     = z
  foldr f z (Si a) = f a z

instance Traversable Talvez where
  traverse _ No     = pure No
  traverse f (Si a) = Si <$> f a

instance (Arbitrary a) => Arbitrary (Talvez a) where
  arbitrary = oneof [return No, fmap Si arbitrary]

instance (Eq a) => EqProp (Talvez a) where
  (=-=) = eq

talvezCheck :: IO ()
talvezCheck = quickBatch $ traversable (undefined :: Talvez (Int, Int, [Int]))

--

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldr _ z Nil         = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

instance Traversable List where
  traverse f = foldr (\x acc -> fmap Cons (f x) <*> acc) (pure Nil)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof
    [ return Nil
    , do
        x  <- arbitrary
        xs <- arbitrary
        return $ Cons x xs
    ]

instance Eq a => EqProp (List a) where
  (=-=) xs ys = toList xs `eq` toList ys

listCheck :: IO ()
listCheck = quickBatch $ traversable (undefined :: List (Int, Int, [Int]))

--

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

threeCheck :: IO ()
threeCheck = quickBatch $ traversable (undefined :: Three Int Int (Int, Int, [Int]))

--

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Foldable (Three' a) where
  foldr f z (Three' _ b1 b2) = f b1 $ f b2 z

instance Traversable (Three' a) where
  traverse f (Three' a b1 b2) = Three' a <$> f b1 <*> f b2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

three'Check :: IO ()
three'Check = quickBatch $ traversable (undefined :: Three' Int (Int, Int, [Int]))

--

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance (Foldable n) => Foldable (S n) where
  foldr f z (S na a) = foldr f (f a z) na

instance (Traversable n) => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Applicative n, Arbitrary a) => Arbitrary (S n a) where
  arbitrary = do
    a  <- arbitrary
    return $ S (pure a) a

instance (Foldable n, Eq a) => EqProp (S n a) where
  (=-=) (S na1 a1) (S na2 a2) = (a1 `eq` a2) .&&. (toList na1 `eq` toList na2)

sCheck :: IO ()
sCheck = quickBatch $ traversable (undefined :: S [] (Int, Int, [Int]))

--

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty      = Empty
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty      = mempty
  foldMap f (Leaf a)   = f a
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

instance Traversable Tree where
  traverse _ Empty      = pure Empty
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = oneof
    [ return Empty
    , fmap Leaf arbitrary
    , do
        l <- arbitrary
        r <- arbitrary
        return $ Node l r
    ]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

treeCheck :: IO ()
treeCheck = quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))
