{-# LANGUAGE InstanceSigs #-}

module Chapter26MonadTrans2 where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

-- 26.14 Chapter Exercices
-- Write the code

rDec :: Num a => Reader a a
rDec = ReaderT (fmap return (\n -> n - 1))

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT (fmap return show)

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \n -> do
  liftIO $ putStrLn $ "Hi: " ++ show n
  return $ n + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  liftIO $ putStrLn $ "Hi: " ++ show s
  return (show s, s + 1)

-- Fix the code

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "Di algo excitante!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn $ "Good, was very excite: " ++ e

-- Hit counter (see "ch26scotty" project)

-- Morra
-- TODO
