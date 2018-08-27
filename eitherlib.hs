module EitherLib where

lefts' :: [Either a b] -> [a]
lefts' = foldr (\a b -> case a of
                            Left x -> x : b
                            _      -> b) []

rights' :: [Either a b] -> [b]
rights' = foldr (\a b -> case a of
                            Right x -> x : b
                            _       -> b) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just $ f b
