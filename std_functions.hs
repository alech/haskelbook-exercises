module StandardFunctions where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) =
    if f x then
        True
    else
        myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem a []     = False
myElem a (x:xs) =
    if a == x then
        True
    else
        myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a xs = any (==a) xs

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- PARTIALS FOLLOWING!
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = myMaximumBy' f x xs
    where
        myMaximumBy' f m []     = m
        myMaximumBy' f m (x:xs) =
            if f x m == GT then
                myMaximumBy' f x xs
            else
                myMaximumBy' f m xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = myMinimumBy' f x xs
    where
        myMinimumBy' f m []     = m
        myMinimumBy' f m (x:xs) =
            if f x m == LT then
                myMinimumBy' f x xs
            else
                myMinimumBy' f m xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
