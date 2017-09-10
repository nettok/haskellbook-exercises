{-# LANGUAGE InstanceSigs #-}

module Chapter26MonadTrans where

import Control.Monad
import Control.Monad.IO.Class

-- 26.2 MaybeT

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure a = MaybeT $ pure $ pure a
  MaybeT fmab <*> MaybeT ma = MaybeT $ fmap (<*>) fmab <*> ma

instance (Monad m) => Monad (MaybeT m) where
  fail _ = MaybeT $ pure Nothing
  return = pure
  (MaybeT mma) >>= famb = MaybeT $ do
    ma <- mma
    case ma of
      Nothing -> return Nothing
      Just a  -> runMaybeT $ famb a

liftMaybeT :: (Monad m) => m a -> MaybeT m a
liftMaybeT = MaybeT . liftM Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = liftMaybeT . liftIO

-- test MaybeT

mt0 :: MaybeT IO String
mt0 = do
  liftIO $ print "Hola"
  --fail ""
  return "mt0"

mt1 :: MaybeT IO String
mt1 = do
  liftIO $ print "Mundo"
  return "mt1"

main :: IO ()
main = do
  mss <- runMaybeT $ mt0 >> mt1
  print mss
  return ()

-- 26.3 EitherT

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance (Applicative m) => Applicative (EitherT e m) where
  pure a = EitherT $ pure $ Right a
  (EitherT mef) <*> (EitherT mea) = EitherT $ fmap (<*>) mef <*> mea

instance (Monad m) => Monad (EitherT e m) where
  fail = EitherT . fail
  return = pure
  (EitherT mea) >>= f = EitherT $ do
    ea <- mea
    case ea of
      Left e  -> return $ Left e
      Right a -> runEitherT $ f a

swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT famc fbmc (EitherT mab) = do
  ab <- mab
  case ab of
    Left a  -> famc a
    Right b -> fbmc b

-- 26.4 ReaderT

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT $ pure $ pure a
  (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ fmap (<*>) rmf <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r -> rma r >>= (\a -> (runReaderT $ f a) r)

-- 26.5 StateT

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

first :: (a -> b) -> (a, s) -> (b, s)
first f (a, s) = (f a, s)

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ (fmap . fmap) (first f) smas

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT smfs) <*> (StateT smas) = StateT $ smfs >=> \(f,s) -> fmap (first f) (smas s)

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT smas) >>= f = StateT $ smas >=> \(a,s) -> runStateT (f a) s
