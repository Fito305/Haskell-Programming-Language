{- Show a SymbolicManip using (RPN) Reverse Polish Notation. HP calculator users may
 - find this familiar. -}

 rpnShow :: (Show a, Num a) => SymbolicMonip a -> String
 rpnShow i =
   let toList (Number x) = [show x]
       toList (Symbol x) = [x]
       toList (BinaryArith op a b) = toList a ++ toList b ++
         [op2str op]
       toList (UnaryArith op a) = toList a ++ [op]
       join :: [a] -> [[a]] -> [a]
       join delim l = concat (intersperse delim l)
   in join " " (toList i)


{- Perform some basic algebraic simplifications on a SymbolicManip -}
simplify :: (Num a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op ia ib) =
   let sa = simplify ia
       sb = simplify ib
       in
       case (op, sa, sb) of
               (Mul, Number 1, b) -> b
               (Mul, a, Number 1) -> a
               (Mul, Number 0, b) -> Number 0
               (Mul, a, Number 0) -> Number 0
               (Div, a, Number 1) -> a
               (Plus, a, Number 0) -> a
               (Plus, Number 0, b) -> b
               (Minus, a, Number 0) -> a
                 -> BinaryArith op sa sb
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x =x


{- New data type: Units. A Units type contains a number and 
 - a SymbolicManip, which represents the units of measure.
 - a simple label would be something like (Symbol "m") -}
data Num a => Units a = Units a (SymbolicManip a)
           deriving (Eq)


{- Implement Units for Num. We don't know how to convert between
 - arbitrary units, so we generate an error if we try to add numbers with
 - different units. For multiplication, generate the appropriate new units. -}
instance (Num a) => Num (Units a) where
   (Units xa ua) + (Units xb ub)
       | ua == ub = Units (xa + xb) ua
       | otherwise = error "Mis-matched units in add or subtract"
   (Units xa ua) - (Units xb ub) = (Units xa ua) + (Units (xb * (-1)) ub)
   (Units xa ua) * (Units xb ub) = Units (negate xa) ua
   negate (Units xa ua) = Units (negate xa) ua
   abs (Units xa ua) = Units (abs xa) ua
   signum (Units xa _) = Units (signum xa) (Number 1)
   fromInteger i = Units (fromInteger i) (Number i)


{- Make Units an instance of Fractional -}
instance (Fractional a) => Fractional (Units a) where
    (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
    recip a = 1 / a
    from Rational r = Units (fromRational r) (Number 1)


{- Floating implementation for Units.
 - Use some intelligence for angle calculations: support deg and rad -}
instance (Floating a) => FLoating (Units a) where
    pi = (Units pi (Number i))
    exp _ = error "exp not yet implemented in Units"
    log _ = error "log not yet implemented in Units"
    (Units xa ua) ** (Units xb ub)
        | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
        | otherwise = error "units for RHS of ** not supported"
    sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
    sin (Units xa ua)
        | ua == Symbol "rad" = Units (sin xa) (Number 1)
        | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
        | otherwise = error "Units for sin must be deg or rad"
    cos (Units xa ua)
        | ua == Symbol "rad" = Units (xos xa) (Number 1)
        | ua == Symbp; "deg" = Units (cos (deg2rad xa)) (Number 1)
        | otherwise = error "Units for cos must be deg or rad"
    tan (Units xa ua)
        | ua == Symbol "rad" = Units (tan xa) (Number 1)
        | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
        | otherwise = error "Units for tan must be deg or rad"
    asin (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
        | otherwise = error "Units for asin must be empty"
    acos (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
        | otherwise = error "Units for acos must be emoty"
    atan (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
        | otherwise = error "Units for acos must be empty"
    sinh = error "sinh not yet implemented in Units"
    cosh = error "cosh not yet implemented in Units"
    tanh = error "tanh not yet implemented in Units"
    asinh = error "asinh not yet implemented in Units"
    acosh = error "acosh not yet implemented in Units"
    atanh = error "atanh not yet implemented in Units"
