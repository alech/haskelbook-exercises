module TenChapterEx where

stops  = "pbtdkg"
vowels = "aeiou"

-- 10.10.1.a
svs = [(a, b, c) | a <- stops, b <- vowels, c <- stops]

-- 10.10.1.b
svs' = [(a, b, c) | a <- stops, b <- vowels, c <- stops, a == 'p']
-- or
-- svs' = [('p', b, c) | b <- vowels, c <- stops]

-- 10.10.1.c
nouns = [ "dog", "human", "mouse", "cat", "house", "computer"]
verbs = [ "eat", "is", "compute", "talk"]

nvn = [(a, b, c) | a <- nouns, b <- verbs, c <- nouns]

-- 10.10.2
-- computes avg length of words in a string of words
-- String -> Int
-- 10.10.3
seekritFunc' x = (toRational $ sum (map length (words x))) / (toRational $ length (words x))

-- 10.10 rewriting functions using folds
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
    if x then
        True
    else
        myOr xs

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) = x || myOr' xs

myOr'' :: [Bool] -> Bool
myOr'' = foldr (\a b -> if a then
                            True
                        else
                            b) False

myOr''' :: [Bool] -> Bool
myOr''' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) =
    if f x then
        True
    else
        myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\a b -> if f a then
                    True
                  else
                    b) False

myAny'' f = foldr (\a b -> f a || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> x == a || b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (==x)

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) [] 

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (\a b -> a ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- THESE ARE PARTIAL!
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> if f a b == GT then a else b) (last xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (last xs) xs
