import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

base = 65521

alder32 xs = helper 1 0 xs
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a + b) `mod` base
                              in helper a' b' xs
          helper a b _      = (b `shiftL` 16) .|. a


-- The sifttL function implements a logical shift left; (.&.) provides 
-- a bitwise "and"; and (.|.) provides a bitwise "or".
-- Our helper function is tail-recursive. When our recursion terminates
-- on the end of the input list, we compute our checksum and return it.
