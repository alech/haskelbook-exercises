tensDigit :: Integral a => a -> a
tensDigit =
    snd . divMod' 10 . fst . divMod' 10
    where divMod' = flip divMod

hunsDigit :: Integral a => a -> a
hunsDigit =
    snd . divMod' 10 . fst . divMod' 100
    where divMod' = flip divMod

-- improved :)
tensDigit' = digitAtPosition 10
hunsDigit' = digitAtPosition 100

digitAtPosition :: Integral a => a -> (a -> a)
digitAtPosition p =
    snd . divMod' 10 . fst . divMod' p
    where divMod' = flip divMod

foldBool :: a -> a -> Bool -> a
foldBool x y b =
    case b of
        False -> x
        True  -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
    | not b     = x
    | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
