{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Integer where
  tooMany n = n > 300000000

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (n, s) = (n + length s) > 42

instance TooMany (Int, Int) where
  tooMany (a, b) = (a + b) > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany a && tooMany b
