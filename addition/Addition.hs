module Addition where

import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greated than 1" $
      ((1::Integer) + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $
      (2::Integer) + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $
      property $ \x -> x + 1 > (x::Integer)
  describe "Multiplication" $ do
    it "3 `mult` 5 is 15" $
      mult (3::Integer) 5 `shouldBe` 15
    it "3 `mult` 0 is 0" $
      mult (3::Integer) 0 `shouldBe` 0
    it "0 `mult` 5 is 15" $
      mult (0::Integer) 5 `shouldBe` 0


mult :: (Eq a, Num a) => a -> a -> a
mult 0 _ = 0
mult _ 0 = 0
mult x y = x + mult x (y - 1)


genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)
