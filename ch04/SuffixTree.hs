suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

-- The pattern `xs@(_:xs')` - is called an *as-pattern*, 
-- an it means "bind the variable `xs` to the 
-- value that matches the right side of the @ symbol."
