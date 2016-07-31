module Main where

import Control.Monad (forever, when)
import Morse (stringToMorse', morseToString')
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (getLine, isEOF)

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "to"   -> inputLoop convertToMorse
        "from" -> inputLoop convertFromMorse
        _      -> argError
    _ -> argError
    where
      argError = do
        progName <- getProgName
        putStrLn $ "error: usage: " ++ progName ++ " [to|from]"
        exitFailure

inputLoop :: (String -> IO()) -> IO ()
inputLoop processInputF = forever $ do
  done <- isEOF
  when done exitSuccess

  line <- getLine
  processInputF line

convertToMorse :: String -> IO ()
convertToMorse line = do
  let morse = stringToMorse' line
  putStrLn . unwords $ morse

convertFromMorse :: String -> IO ()
convertFromMorse line = do
  let morse = words line
  let string = morseToString' morse
  putStrLn string
