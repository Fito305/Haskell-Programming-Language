append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs
