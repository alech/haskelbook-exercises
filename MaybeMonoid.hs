module MaybeMonoid where

data Optional a =
      Nada
    | Only a
    deriving (Eq, Show)

fromOptional :: a -> Optional a -> a
fromOptional a Nada     = a
fromOptional _ (Only a) = a

instance Monoid a => Monoid (Optional a) where
    mempty  = Only mempty
    mappend a b = Only $ mappend (fromOptional mempty a) (fromOptional mempty b)
