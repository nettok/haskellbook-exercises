module Main where

import Control.Monad (forever)
import Data.Char (toLower, isAscii)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

type Guessed = Bool

data GuessLetter = GuessLetter Char Guessed deriving (Eq, Show)

type Puzzle = [GuessLetter]

-- main game loop

main :: IO ()
main = do
  puzzleWord <- randomWord'
  let puzzle = makePuzzle puzzleWord
  guessLoop puzzle

guessLoop :: Puzzle -> IO ()
guessLoop puzzle =
  if isSolved puzzle then putStrLn "Puzzle is solved" >> exitSuccess else
    forever $ do
      _ <- putStrLn $ "Guess the word: " ++ showPuzzle puzzle
      _ <- putStr  "What is your guess? "
      l <- getLine
      case l of
        "exit"  -> exitSuccess
        "debug" -> print puzzle
        [c]     -> tryLetterGuess puzzle c >>= guessLoop
        _       -> putStrLn "You must guess a single letter, or write \"exit\" to leave the game."

tryLetterGuess :: Puzzle -> Char -> IO Puzzle
tryLetterGuess puzzle c
  | isLetterGuessed puzzle c = putStrLn "Letter is already guessed" >> return puzzle
  | otherwise                = return $ guess puzzle c


-- generate a random puzzle word

allWords :: IO WordList
allWords = do
  dict <- readFile "data/espanol.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return $ map (map toLower) $ filter (\w -> validWordLength w && all isAscii w) aw
  where validWordLength w = let wordLength = length w
                            in minWordLength >= wordLength && wordLength <= maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  i <- randomRIO (0, length wl - 1)
  return (wl !! i)

randomWord' :: IO String
randomWord' = gameWords >>= randomWord


-- pure functions for puzzle solving

guess :: Puzzle -> Char -> Puzzle
guess puzzle c = foldr (\c' acc -> guessLetter c' : acc) [] puzzle
  where guessLetter gl@(GuessLetter c' _) = if c' == c then GuessLetter c' True else gl

guessAll :: Puzzle -> String -> Puzzle
guessAll = foldl guess

makePuzzle :: String -> Puzzle
makePuzzle = map (\c -> GuessLetter (toLower c) (c == ' '))

showPuzzle :: Puzzle -> String
showPuzzle puzzle = intersperse ' ' $ map showGuessLetter puzzle

showGuessLetter :: GuessLetter -> Char
showGuessLetter (GuessLetter c guessed) = if guessed then c else '_'

isSolved :: Puzzle -> Bool
isSolved = all (\(GuessLetter _ b) -> b::Bool)

isLetterGuessed :: Puzzle -> Char -> Bool
isLetterGuessed puzzle c = any (\(GuessLetter c' guessed) -> c' == c && guessed) puzzle
