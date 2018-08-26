module TheVowel where

startsWithVowel :: String -> Bool
startsWithVowel ('a':_) = True
startsWithVowel ('e':_) = True
startsWithVowel ('i':_) = True
startsWithVowel ('o':_) = True
startsWithVowel ('u':_) = True
startsWithVowel _ = False

countBeforeVowel :: String -> Integer
countBeforeVowel =
    sum . c' . words
    where
        c' :: [String] -> [Integer]
        c' ("the":x:xs) =
            if startsWithVowel x then
                1 : c' (x:xs)
            else
                0 : c' (x:xs)
        c' [] = []
        c' (x:xs) = c' xs
