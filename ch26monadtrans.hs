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
-- TODO
