module Chapter11Cipher where

import Data.Char

caesarChar :: Int -> Char -> Char
caesarChar rShift char = chr $ ord char + rShift

encrypt :: Int -> String -> String
encrypt rShift = map (caesarChar rShift)

decrypt :: Int -> String -> String
decrypt rShift = map (caesarChar (-rShift))

encrypt' :: String -> String -> String
encrypt' password plainText = map g f
  where f             = zip (cycle password) plainText
        g (key, char) = caesarChar (ord key - ord 'A') char

decrypt' :: String -> String -> String
decrypt' password cypher = map g f
  where f             = zip (cycle password) cypher
        g (key, char) = caesarChar (negate $ ord key - ord 'A') char
