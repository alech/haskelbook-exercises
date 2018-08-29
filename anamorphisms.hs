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

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a =
    case f a of
        Nothing        -> Leaf
        Just (l, v, r) -> Node (unfold f l) v (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
    unfold (buildLevels n) 0
    where
        buildLevels :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
        buildLevels n m =
            if m >= n then
                Nothing
            else
                Just (m + 1, m, m + 1)
