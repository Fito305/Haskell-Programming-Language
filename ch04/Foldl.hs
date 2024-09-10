foldl :: (a -> b -> a) -> a -> [b] -> a

fodl step zero (x:xs) = foldl step (step zero x) xs
fodl _ zero [] = zero
