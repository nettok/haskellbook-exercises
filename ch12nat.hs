module Chapter12Nat where

import Data.Either
import Data.Either.Extra
import Data.Maybe


data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ (integerToNat (n - 1))


catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs =
  let cms = Chapter12Nat.catMaybes xs
  in if length cms == length xs then Just cms else Nothing


lefts' :: [Either a b] -> [a]
lefts' = map fromLeft .  filter isLeft

rights' :: [Either a b] -> [b]
rights' = map fromRight . filter isRight

lefts'' :: [Either a b] -> [a]
lefts'' = foldr (\x acc -> if isLeft x then fromLeft x : acc else acc ) []


iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f x =
  case f x of
    Just (y, z) -> y : unfoldr' f z
    Nothing     -> []

iterate'' :: (a -> a) -> a -> [a]
iterate'' f = unfoldr' (\x -> Just (x, f x))


data BTree a =
    Leaf
  | Node (BTree a) a (BTree a)
  deriving (Eq, Ord, Show)

btUnfold :: (a -> Maybe (a, b, a)) -> a -> BTree b
btUnfold f x =
  case f x of
    Just (l, v, r) -> Node (btUnfold f l) v (btUnfold f r)
    Nothing        -> Leaf

btBuild :: Integer -> BTree Integer
btBuild n = btUnfold (\x -> if x < n then Just (x + 1, x, x + 1) else Nothing) 0
