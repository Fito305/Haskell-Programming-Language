-- You might wonder where the variable name `xs` comes from
-- in the Haskell function. This is a common naming pattern for lists. 
-- You can read the s as a suffix, so the name is essentially "plural of x".
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n -1) (tail xs)
