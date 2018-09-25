qty :: [a] -> [(a, Int)]
qty l = nub (map (\x -> (x, length (filter (==x) l))) l)

foldr (++) []

