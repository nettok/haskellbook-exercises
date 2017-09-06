{-# LANGUAGE InstanceSigs #-}

module Chapter25ComposeTypes where

import Control.Monad.IO.Class
import Prelude hiding (Either(..))

newtype Identity a =
  Identity  { runIdentity :: a }
  deriving (Eq, Show)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ fmap (<*>) f <*> a

-- 25.6 Exercises: Compose Instances

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = foldMap (foldMap f) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga

-- And now for something completly different

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1
data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

-- 2
newtype Const a b = Const a deriving (Eq, Show)

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

-- 3
data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 4
data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- 5
newtype SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

-- 6
data Quadriceps a b c d = Quadzzz a b c d deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- 7
data Either a b = Left a | Right b

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)


-- 25.8 Implement IdentityT Functor, Applicative and Monad instances (page 961)
-- `newtype Identity a` is already implemented above
-- `instance Functor Identity` is already implemented above

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= famb = famb a

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance (Applicative m) => Applicative (IdentityT m) where
  pure a = IdentityT $ pure a
  IdentityT mfa <*> IdentityT ma = IdentityT $ mfa <*> ma

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  IdentityT ma >>= fmb = IdentityT $ ma >>= runIdentityT . fmb

instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO

-- test

id0 :: IdentityT IO Int
id0 = do
  liftIO $ print "hola"
  return 4

id1 :: Int -> IdentityT IO Int
id1 n = do
  liftIO $ print "mundo"
  return $ n + 5

main :: IO ()
main = do
  n <- runIdentityT $ id0 >>= id1
  print n
  return ()
