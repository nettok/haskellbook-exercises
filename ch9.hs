module Chapter9 where

import Data.Char


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


filterUpper :: String -> String
filterUpper = filter isUpper


capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs


upper :: String -> String
upper = map toUpper


upperHead :: String -> Char
upperHead = toUpper . head


main :: IO ()
main = putStrLn "Hola loco!"


-- TODO NEXT:  Ch9 - Cypher!
