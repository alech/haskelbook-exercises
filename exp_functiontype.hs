module ExponentialFunctionType where

data Quantum = Yes | No | Both

convert0 :: Quantum -> Bool
convert0 Yes  = False
convert0 No   = False
convert0 Both = False

convert1 :: Quantum -> Bool
convert1 Yes  = False
convert1 No   = False
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = False
convert2 No   = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes  = False
convert3 No   = True
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes  = True
convert4 No   = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes  = True
convert5 No   = False
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes  = True
convert6 No   = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes  = True
convert7 No   = True
convert7 Both = True

-- Either Quad Quad : 8
-- (Quad, Quad) : 4*4 = 16
-- Quad -> Quad : 4^4 = 256
-- prodTBool :: (Bool, Bool, Bool) = 2*2*2 = 8
-- gTwo :: Bool -> Bool -> Bool = (2^2)^2 = 16
-- fTwo :: Bool -> Quad -> Quad = (4^2)^4 = 65536
