{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter11SumTypes where


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n

instance TooMany (Int, Int) where
  tooMany (m, n) = tooMany (m + n)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (m, n) = tooMany m || tooMany n || tooMany (m + n)

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
