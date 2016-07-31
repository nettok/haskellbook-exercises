module Chapter10Database where

import Data.Time


data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hola Mundo!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr extractUtcTime []
  where extractUtcTime (DbDate utcTime) acc = utcTime : acc
        extractUtcTime _                acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr extractInteger []
  where extractInteger (DbNumber int) acc = int : acc
        extractInteger _              acc = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral . sum) numbers / (fromIntegral . length) numbers
  where numbers = filterDbNumber xs

----

stops  = "pbtdkg"
vowels = "aeiou"

asd :: [(Char, Char, Char)]
asd = filter (\x -> getSt1 x == 'p') [(st1, v, st2) | st1 <- stops, v <- vowels, st2 <- stops]
  where getSt1 (st1, _, _) = st1

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\x acc -> acc || p x) False

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []
