module CountVowels where

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _   = False

countVowels :: String -> Integer
countVowels = foldr (\a b -> if isVowel a then b + 1 else b) 0
