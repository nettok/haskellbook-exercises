module Chapter12 where

import Data.Char
import Data.Maybe

notThe :: String -> Maybe String
notThe w =
  case map toLower w of
    "the" -> Nothing
    _     -> Just w

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

data WordType =
    The
  | Vowel
  | Consonant
  deriving (Eq)

wordType :: String -> WordType
wordType w =
  case map toLower w of
    "the"      -> The
    lowercased ->
      case head lowercased of
        c
          | c `elem` ['a', 'e', 'i', 'o', 'u'] -> Vowel
          | otherwise                          -> Consonant

pairs :: [a] -> [(a,a)]
pairs []       = []
pairs [_]      = []
pairs (x:y:xs) = (x,y) : pairs (y:xs)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = fromIntegral . length . filter (\p -> p == (The, Vowel)) . pairs . map wordType . words
