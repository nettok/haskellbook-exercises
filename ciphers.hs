module Ciphers where

import Data.Char

caesarChar :: Int -> Char -> Char
caesarChar rShift char = chr $ ord char + rShift

encrypt :: Int -> String -> String
encrypt rShift = map (caesarChar rShift)

decrypt :: Int -> String -> String
decrypt rShift = map (caesarChar (-rShift))


or' :: [Bool] -> Bool
or' []      = False
or' (x:xs) = x || or' xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ []     = False
any' p (x:xs) = p x || any' p xs

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs
