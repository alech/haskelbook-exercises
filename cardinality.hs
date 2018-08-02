-- 11.10
-- data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
-- cardinality 4 : Big False, Big True, Small False, Small True
import Data.Int

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)
