module MaybeLib where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

maaybee :: b -> (a -> b) -> Maybe a -> b
maaybee b _ Nothing = b
maaybee _ f (Just a)  = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe x = maaybee x id

listToMaybe :: [a] -> Maybe a
listToMaybe xs
    | null xs   = Nothing
    | otherwise = Just $ head xs

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\a b -> case a of
                            Nothing  -> b
                            (Just x) -> x : b) []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if any isNothing xs then
                Nothing
               else
                Just $ catMaybes xs
