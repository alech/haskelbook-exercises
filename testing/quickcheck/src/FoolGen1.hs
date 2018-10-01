module FoolGen1 where

import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen =
    elements [Fulse, Frue]
