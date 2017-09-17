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

exciteIsValid :: String -> Bool
exciteIsValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ exciteIsValid v
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
-- TODO: the current implementation is not really "Morra", but a basic skeleton for a turn based game

data GameState = GameState {
    p1 :: Player
  , p2 :: Player
  , turn :: Turn
  , winner :: Maybe Player
  }

data Player = Player {
    name :: String
  , score :: Score
  , isHuman :: Bool
  } deriving (Show)

data Turn = Turn {
    currentPlayer :: GameState -> Player
  , updateCurrentPlayer :: GameState -> Player -> GameState
  }

type Score = Int

initialGameState :: GameState
initialGameState = GameState {
    p1 = Player { name = "p1", score = 0, isHuman = True }
  , p2 = Player { name = "p2", score = 0, isHuman = False }
  , turn = Turn {
             currentPlayer = p1
           , updateCurrentPlayer = \gs p -> gs { p1 = p }
           }
  , winner = Nothing
  }

alternateTurns :: GameState -> GameState
alternateTurns gs = if playerName gs == name (p1 gs)
                      then gs { turn = Turn {
                                         currentPlayer = p2
                                       , updateCurrentPlayer = \gs' p -> gs' { p2 = p }
                                       }
                              }
                      else gs { turn = Turn {
                                         currentPlayer = p1
                                       , updateCurrentPlayer = \gs' p -> gs' { p1 = p }
                                       }
                              }

gameLoop :: StateT GameState IO ()
gameLoop = do
  playerInput
  gs <- get
  when (playerScore gs >= 10) playerWins
  gs' <- get
  case winner gs' of
    Nothing -> changeTurn >> gameLoop
    Just _ -> return ()

changeTurn :: StateT GameState IO ()
changeTurn = get >>= (put . alternateTurns)

playerInput :: StateT GameState IO ()
playerInput = do
  gs <- get
  points <- liftIO $ (if isHuman $ player gs then humanInput else robotInput) gs
  incrPlayerScore points

humanInput :: GameState -> IO Score
humanInput gs = do
  putStr (playerName gs ++ ": ")
  inputS <- getLine
  return $ read inputS

robotInput :: GameState -> IO Score
robotInput _ = return 1 -- TODO: improve

incrPlayerScore :: Score -> StateT GameState IO ()
incrPlayerScore n = do
  gs <- get
  put $ updatePlayerWith gs (\p -> p { score = playerScore gs + n })

playerWins :: StateT GameState IO ()
playerWins = get >>= (put . (\gs -> gs { winner = Just $ player gs }))

player :: GameState -> Player
player gs = currentPlayer (turn gs) gs

playerName :: GameState -> String
playerName = name . player

playerScore :: GameState -> Score
playerScore = score . player

updatePlayer :: GameState -> Player -> GameState
updatePlayer gs = updateCurrentPlayer (turn gs) gs

updatePlayerWith :: GameState -> (Player -> Player) -> GameState
updatePlayerWith gs f = updatePlayer gs (f $ player gs)

main :: IO ()
main = do
  (_, gs) <- runStateT gameLoop initialGameState
  putStrLn ""
  putStrLn "Resultados:"
  print $ p1 gs
  print $ p2 gs
  putStrLn $ "Ganador: " ++ show (winner gs)
