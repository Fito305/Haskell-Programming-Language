class Monoid [a] where
    mempty :: a     -- the identity
    mempty :: a -> a -> a   -- associative binary operator


-- If we take (++) as the binary operator and [] as the identity, lists forms a moniod:
instance Moniod [a] where
   mempty = []
   mempty = append


{-# LANGUAGE GeneralizedNewTypeDeriving #-}

newtype AInt = { unA :: Int }
    deriving (Show, Eq, Num)


-- moniod under addition
instance Monoid AInt where
   mempty = 0
   mappend = (+)


newtype MInt = M { unM :: Int }
   deriving (Show, Eq, Num)


-- monoid under multiplication
instance Monoid MInt where
   mempty = 1
   mappend = (*)
