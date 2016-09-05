module Chapter23State where

import Control.Applicative (liftA3)
import Control.Monad
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Maybe
import System.Random

data Die = Die1 | Die2 | Die3 | Die4 | Die5 | Die6 deriving (Eq, Show)

intToDie :: Int -> Maybe Die
intToDie n =
  case n of
    1 -> Just Die1
    2 -> Just Die2
    3 -> Just Die3
    4 -> Just Die4
    5 -> Just Die5
    6 -> Just Die6
    _ -> Nothing

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  ((fromJust . intToDie) d1, (fromJust . intToDie) d2, (fromJust . intToDie) d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return ((fromJust . intToDie) n, s)

rollDie' :: State StdGen Die
rollDie' = (fromJust . intToDie) <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

rollDieNTimes :: Int -> State StdGen [Die]
rollDieNTimes n = replicateM n rollDie'

-- Exercises: Roll Your Own (page 907)

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where go :: Int -> Int -> StdGen -> Int
        go suma count gen
          | suma >= n  = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
              in go (suma + die) (count + 1) nextGen

rollsToGetNLogged :: Int -> StdGen -> (Int, [Die])
rollsToGetNLogged n = go 0 0 []
  where go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go suma count rolls gen
          | suma >= n  = (count, rolls)
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
              in go (suma + die) (count + 1) (rolls ++ [(fromJust . intToDie) die]) nextGen

--

newtype Estado s a = Estado { runEstado :: s -> (a, s) }

instance Functor (Estado s) where
  fmap f (Estado g) = Estado h
    where h s = let (a, nextS) = g s
                in (f a, nextS)

instance Applicative (Estado s) where
  pure a = Estado (\s -> (a, s))

  Estado f <*> Estado g = Estado h
    where h s = let (fab, s') = f s
                    (a, s'')  = g s'
                in (fab a, s'')

instance Monad (Estado s) where
  return = pure

  Estado f >>= g = Estado h
    where h s = let (a, s') = f s
                in runEstado (g a) s'

-- 23.7 Get a coding job with one weird trick (909)

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = show n

main :: IO ()
main = mapM_ (putStrLn . fizzBuzz) [1..100]

-- 23.8 Chapter exercices (page 913)

get' :: State s s
get' = StateT { runStateT = \s -> Identity (s, s) }

put' :: s -> State s ()
put' s = StateT { runStateT = \_ -> Identity ((), s) }

exec :: State s a -> s -> s
exec (StateT sa) s = snd $ runIdentity (sa s)

eval :: State s a -> s -> a
eval (StateT sa) s = fst $ runIdentity (sa s)

modify' :: (s -> s) -> State s ()
modify' ss = StateT { runStateT = \s -> Identity ((), ss s) }
