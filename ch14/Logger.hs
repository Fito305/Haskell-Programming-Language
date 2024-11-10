module Logger
    (
       Logger
     , Log
     , runLogger
     , record
    ) where


globToRegex :: String -> Logger String
type Log = [String]
runLogger :: Logger a -> (a, Log)
record :: String -> Logger ()


globToRegex cs =
    globToRegex' cs >>= \ds ->
    return ('^' :ds)


-- Remember the type of (>>=): it extracts the value on the left from its Logger wrapper, 
-- and passes the unwrapped value to the function on the right. The function on the right 
-- must, in turn, wrap its result with the Logger wrapper. This is exactly what return does.


globToRegex' :: String -> Logger String
globToRegex' "" = return "$"

globToRegex' ('?' :cs) =
    record "any" >>
    globToRegex' cs >>= \ds ->
    return ('.' :ds)


globToRegex' ('*' :cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)


globToRegex' ('[' : '!' :c:cs) =
    record "character class, negative" >>
    charClass cs >>= ds ->
    return ("[^" ++ c :ds)
globToRegex' ('[' :c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c :ds)
globToRegex' ('[' :+) =
    fail "unterminated character class"


liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= \i ->
            return (f i)


charClass (']' :cs) = (']' :) `liftM` globToRegex' cs
charClass (c:cs)  (c:) `liftM` charClass cs


{- liftM2 - It executes the first action, then the secoond, and then combines their results using the  
    pure function f, and wraps that result. -}
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 =
    m1 >>= \a ->
    m2 >>= \b ->
    return (f a b)

-- It's a pair, where the first element is the result of an action, and the second is a list of 
-- messages logged while that action was run.
newtype Logger a = Logger { execLogger :: (a, log) }

runLogger = execLogger
record s = Logger ((), [s])


instance Monad Logger where
   return a = Logger (a, [])


-- Slightly more interesting is (>>=), which is the heart of the monad. It combines
-- an action and a monadic function to give a new result and a new log:
-- (>>=) :: Logger a -> (a -> Logger b) -> Logger b
m >>= k = let (a, w) = execLogger m
              n      = k a
              (b, x) = execLogger n
          in Logger (b, w ++ x)

