-- Let's look at the singature of the take function,
-- It's pretty clear that there's something going on with an Int and some lists, 
-- but why are there two -> symbols in the signature? Haskell groups this chain 
-- of arrows from right to left; that is ``` -> is right-assoiciative```.
-- If we introduce parenthesis, we can make it clearer how this type
-- signature is interpreted.
take :: Int -> ([a] -> [a])
