module Chapter11Jammin where

import Data.List

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  | Banana
  deriving (Eq, Show, Ord)

data JamJars = Jam
  {
    fruit :: Fruit
  , cannedCount :: Int
  } deriving (Eq, Show, Ord)

allJam :: [JamJars]
allJam = [Jam Peach 3, Jam Apple 2, Jam Plum 0, Jam Blackberry 4, Jam Banana 5, Jam Plum 1, Jam Apple 6]

countCannedJamJars :: [JamJars] -> Int
countCannedJamJars = foldr (\jj acc -> cannedCount jj + acc) 0

mostJam :: [JamJars] -> JamJars
mostJam jjs = foldr maxJj (head jjs) jjs
  where maxJj x y = if cannedCount x > cannedCount y then x else y

compareJamFruit :: JamJars -> JamJars -> Ordering
compareJamFruit jj1 jj2 = compare (fruit jj1) (fruit jj2)

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\jj1 jj2 -> fruit jj1 == fruit jj2) . sort
