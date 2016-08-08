module Morse
    ( Morse
    , letterToMorse
    , morseToLetter
    , charToMorse
    , morseToChar
    , stringToMorse
    , morseToString
    , stringToMorse'
    , morseToString'
    ) where


import Data.Char (toLower)
import Data.Maybe

import qualified Data.Map as M


type Morse = String

letterToMorse :: M.Map Char Morse
letterToMorse = M.fromList
  [ ('a', ".-")
  , ('b', "-...")
  , ('c', "-.-.")
  , ('d', "-..")
  , ('e', ".")
  , ('f', "..-.")
  , ('g', "--.")
  , ('h', "....")
  , ('i', "..")
  , ('j', ".---")
  , ('k', "-.-")
  , ('l', ".-..")
  , ('m', "--")
  , ('n', "-.")
  , ('o', "---")
  , ('p', ".--.")
  , ('q', "--.-")
  , ('r', ".-.")
  , ('s', "...")
  , ('t', "-")
  , ('u', "..-")
  , ('v', "...-")
  , ('w', ".--")
  , ('x', "-..-")
  , ('y', "-.--")
  , ('z', "--..")
  , ('1', ".----")
  , ('2', "..---")
  , ('3', "...--")
  , ('4', "....-")
  , ('5', ".....")
  , ('6', "-....")
  , ('7', "--...")
  , ('8', "---..")
  , ('9', "----.")
  , ('0', "-----")
  ]

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldWithKey (\c m inverseMap -> M.insert m c inverseMap) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup (toLower c) letterToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse

morseToString :: [Morse] -> Maybe String
morseToString = traverse morseToChar

stringToMorse' :: String -> [Morse]
stringToMorse' = map (fromMaybe "?" . charToMorse)

morseToString' :: [Morse] -> String
morseToString' = map (fromMaybe '?' . morseToChar)
