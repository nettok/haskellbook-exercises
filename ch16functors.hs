{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter16Functors where

import Data.Functor
import GHC.Arr
import Test.QuickCheck
import Test.QuickCheck.Function


data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

functorCompose' :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = fmap (g . f) x == (fmap g . fmap f $ x)

type FunctorComposeInt = [Int] -> Fun Int Int -> Fun Int Int -> Bool

qcFCInt :: IO ()
qcFCInt = quickCheck (functorCompose' :: FunctorComposeInt)

-- 16.10 Exercises: Instances of Func
-- 1

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

type FunctorComposeIdentityInt = Identity Int -> Fun Int Int -> Fun Int Int -> Bool

qcFCIdentityInt :: IO ()
qcFCIdentityInt = quickCheck (functorCompose' :: FunctorComposeIdentityInt)

-- 2

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

type FunctorComposePairInt = Pair Int -> Fun Int Int -> Fun Int Int -> Bool

qcFCPairInt :: IO ()
qcFCPairInt = quickCheck (functorCompose' :: FunctorComposePairInt)

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

type FunctorComposeTwoTrivialInt = Two Trivial Int -> Fun Int Int -> Fun Int Int -> Bool

qcFCTwoTrivialInt :: IO ()
qcFCTwoTrivialInt = quickCheck (functorCompose' :: FunctorComposeTwoTrivialInt)

-- 4

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

type FunctorComposeThreeTrivialInt = Three Trivial Trivial Int -> Fun Int Int -> Fun Int Int -> Bool

qcFCThreeTrivialInt :: IO ()
qcFCThreeTrivialInt = quickCheck (functorCompose' :: FunctorComposeThreeTrivialInt)

-- 5

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

type FunctorComposeThree'TrivialInt = Three' Trivial Int -> Fun Int Int -> Fun Int Int -> Bool

qcFCThree'TrivialInt :: IO ()
qcFCThree'TrivialInt = quickCheck (functorCompose' :: FunctorComposeThree'TrivialInt)

-- 6

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

type FunctorComposeFourTrivialInt = Four Trivial Trivial Trivial Int -> Fun Int Int -> Fun Int Int -> Bool

qcFCFourTrivialInt :: IO ()
qcFCFourTrivialInt = quickCheck (functorCompose' :: FunctorComposeFourTrivialInt)

-- 7

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z

type FunctorComposeFour'TrivialInt = Four' Trivial Int -> Fun Int Int -> Fun Int Int -> Bool

qcFCFour'TrivialInt :: IO ()
qcFCFour'TrivialInt = quickCheck (functorCompose' :: FunctorComposeFour'TrivialInt)

-- 16.11 Ignoring possibilities

data Talvez a =
    Si a
  | No
  deriving (Eq, Show)

instance Functor Talvez where
  fmap f (Si x) = Si (f x)
  fmap _ No     = No

instance (Arbitrary a) => Arbitrary (Talvez a) where
  arbitrary = oneof [fmap Si arbitrary, return No]

type FunctorComposeTalvezInt = Talvez Int -> Fun Int Int -> Fun Int Int -> Bool

qcFCTalvezInt :: IO ()
qcFCTalvezInt = quickCheck (functorCompose' :: FunctorComposeTalvezInt)

--

data Validation a b =
    Invalid a
  | Valid b
  deriving (Eq, Show)

instance Functor (Validation a) where
  fmap f (Valid x)   = Valid (f x)
  fmap _ (Invalid e) = Invalid e

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [fmap Invalid arbitrary, fmap Valid arbitrary]

type FunctorComposeValidationIntInt = Validation Int Int -> Fun Int Int -> Fun Int Int -> Bool

qcFCValidationIntInt :: IO ()
qcFCValidationIntInt = quickCheck (functorCompose' :: FunctorComposeValidationIntInt)

-- Natural Transformations

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing  = []
maybeToList (Just x) = [x]

maybeToList' :: Maybe a -> [a]
maybeToList' Nothing  = []
maybeToList' (Just x) = [x]

-- 16.16 Functors are unique to a datatype + 16.17 Exercices
-- 1.4

newtype Mu f = InF { outF :: f (Mu f) }
-- TODO ???

instance Functor (f Mu) where
  fmap f fMua = undefined

-- 1.5

data D = D (Array Word Word) Int Int deriving (Eq, Show)
-- TODO ???

-- 2.1

data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b

-- 2.2

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 2.3

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- 3.1

data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a

-- 3.2

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3.3

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

-- 3.4

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 3.5

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut (fmap g fa)

-- 3.6

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 3.7

data IgnoreOne f g a b = IgnoreSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

-- 3.8

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 3.9

data List a = Nil | Cons a (List a) deriving (Eq , Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

toList :: [a] -> List a
toList []     = Nil
toList (x:xs) = Cons x (toList xs)

-- 3.10

data GoatLoard a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLoard a) (GoatLoard a) (GoatLoard a)

instance Functor GoatLoard where
  fmap _ NoGoat                  = NoGoat
  fmap f (OneGoat a)             = OneGoat (f a)
  fmap f (MoreGoats gl1 gl2 gl3) = MoreGoats (fmap f gl1) (fmap f gl2) (fmap f gl3)

-- 3.11

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sa)   = Read (f . sa)
