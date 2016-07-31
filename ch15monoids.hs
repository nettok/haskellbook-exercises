module Chapter15Monoids where

import qualified Data.Monoid as M
import qualified Data.Semigroup as S
import Test.QuickCheck hiding (Failure, Success)

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty      = Nada
  mappend Nada Nada         = Nada
  mappend Nada oy@(Only _)  = oy
  mappend ox@(Only _) Nada  = ox
  mappend (Only x) (Only y) = Only (x `mappend` y)

-- Madness!!!

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  mconcat [e, "! he said ", adv, " as he jumped into his car ",
           noun, " and drove off with this ", adj, " wife."]

-- QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc x y z = x M.<> (y M.<> z) == (x M.<> y) M.<> z

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (mempty M.<> x) == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = (x M.<> mempty) == x

qcMonoidAllString :: IO ()
qcMonoidAllString = do
  quickCheck (monoidAssoc :: String -> String -> String -> Bool)
  quickCheck (monoidLeftIdentity :: String -> Bool)
  quickCheck (monoidRightIdentity :: String -> Bool)

-- Bull (not really a monoid) (proof with QuickCheck)

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty      = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

qcMonoidAllBull :: IO ()
qcMonoidAllBull = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidRightIdentity :: Bull -> Bool)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)

-- Maybe another Monoid

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = oneof [fmap (First' . Only) arbitrary, return $ First' Nada]

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada)         = First' Nada
  mappend (First' Nada) foy@(First' (Only _)) = foy
  mappend fox@(First' (Only _)) (First' Nada) = fox
  mappend (First' (Only x)) (First' (Only _)) = First' (Only x)

type FirstMappend = First' String -> First' String -> First' String -> Bool

qcMonoidAllFirst' :: IO ()
qcMonoidAllFirst' = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)

-- Semigroup exercises
-- 1

data Trivial = Trivial deriving (Eq, Show)

instance S.Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool
semigroupAssoc x y z = x S.<> (y S.<> z) == (x S.<> y) S.<> z

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

qcSemiTrivial :: IO ()
qcSemiTrivial = quickCheck (semigroupAssoc :: TrivialAssoc)

-- 2

newtype Identity a = Identity a deriving (Eq, Show)

instance (S.Semigroup a) => S.Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x S.<> y)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

type IdentityStringAssoc = Identity String -> Identity String -> Identity String -> Bool

qcSemiIdentityString :: IO ()
qcSemiIdentityString = quickCheck (semigroupAssoc :: IdentityStringAssoc)

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (Two a b) where
  Two ax bx <> Two ay by = Two (ax S.<> ay) (bx S.<> by)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

type TwoStringStringAssoc = Two String String -> Two String String -> Two String String -> Bool

qcSemiTwoStringString :: IO ()
qcSemiTwoStringString = quickCheck (semigroupAssoc :: TwoStringStringAssoc)

-- 6

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance S.Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj (arbitrary :: Gen Bool)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

qcSemiBoolConj :: IO ()
qcSemiBoolConj = quickCheck (semigroupAssoc :: BoolConjAssoc)

-- 8

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance S.Semigroup (Or a b) where
  Snd x <> _ = Snd x
  _ <> Snd y = Snd y
  _ <> Fst y = Fst y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [fmap Fst arbitrary, fmap Snd arbitrary]

type OrAssocString = Or String String -> Or String String -> Or String String -> Bool

qcSemiOrString :: IO ()
qcSemiOrString = quickCheck (semigroupAssoc :: OrAssocString)

-- 9 -- TODO: don't know how to quickCheck this

newtype Combine a b = Combine { unCombine :: a -> b }

instance (S.Semigroup b) => S.Semigroup (Combine a b) where
  Combine fx <> Combine fy = Combine (fx S.<> fy)

-- 10

data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)

instance (S.Semigroup a) => S.Semigroup (Validation a b) where
  Failure x <> Failure y = Failure (x S.<> y)
  Failure x <> _         = Failure x
  _         <> Failure y = Failure y
  Success _ <> Success y = Success y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [fmap Failure arbitrary, fmap Success arbitrary]

type ValidationStrStr = Validation String String
type ValidationAssocStrings = ValidationStrStr -> ValidationStrStr -> ValidationStrStr -> Bool

qcSemiValidationStr :: IO ()
qcSemiValidationStr = quickCheck (semigroupAssoc :: ValidationAssocStrings)

-- Monoid exercises
--  1

instance Monoid Trivial where
  mempty = Trivial
  mappend = (S.<>)

qcMonoidTrivial :: IO ()
qcMonoidTrivial = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

-- 2

instance (S.Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity x) (Identity y) = Identity (x S.<> y)

qcMonoidIdentityString :: IO ()
qcMonoidIdentityString = do
  quickCheck (semigroupAssoc :: IdentityStringAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

-- 4

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (S.<>)

qcMonoidBoolConj :: IO ()
qcMonoidBoolConj = do
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

-- 8

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend (Mem f) (Mem g) = Mem (\s -> (fst (f s) M.<> fst (g s), (snd . g . snd . f) s))

f' :: Mem Integer String
f' = Mem $ \s -> ("hi", s + 1)

checkMem :: IO ()
checkMem = do
  print $ runMem (f' M.<> mempty) 0
  print $ runMem (mempty M.<> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' M.<> mempty) 0 == runMem f' 0
  print $ runMem (mempty M.<> f') 0 == runMem f' 0
