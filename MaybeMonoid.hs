module MaybeMonoid where

import Data.Monoid

data Optional a =
      Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty  = Only mempty
    mappend Nada     (Only b) = Only $ mempty <> b
    mappend (Only a) Nada     = Only $ a <> mempty
    mappend (Only a) (Only b) = Only $ a <> b
    mappend Nada Nada         = Only $ mempty
