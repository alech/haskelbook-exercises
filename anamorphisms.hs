module Anamorphisms where

myIterate :: (a -> a) -> a -> [a]
myIterate f a =
    a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
    case f b of
        Nothing         -> []
        Just (a, nextA) -> a : myUnfoldr f (nextA)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f a =
    myUnfoldr (\a -> Just (a, f a)) a
