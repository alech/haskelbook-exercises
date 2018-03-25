module WaxOn where

waxOn = x * 5
    where
        x = y ^ 2
        y = z + 8
        z = 7

triple x = 3 * x
waxOff x = (triple x)^1001
