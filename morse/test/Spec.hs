module Main where

import qualified Data.Map as M
import Data.Char
import Morse
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "charToMorse and morseToChar" $ do
    it "convert correctly back and forth for allowedChars" $
      forAll charGen (\c -> (charToMorse c >>= morseToChar) == Just c)

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.keys morseToLetter
