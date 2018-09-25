--Ex 1
invert :: [a] -> [a]
invert [] = []
invert (x:xs) = invert xs ++ [x]

--Ex 2
addall :: (Num a) => [a] -> a
addall [] = 0
addall (x:xs) = x + addall xs

--Ex 3
maxlist :: (Ord a) => [a] -> a
maxlist [x] = x 
maxlist (x:xs) = 
    let biggest = maxlist xs
    in if biggest < x then x else biggest

--Ex 4
fibonacci :: (Integral a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

--Ex 5
fliperoo :: [a] -> [a]
fliperoo [] = []
fliperoo [x] = [x]
fliperoo (x:y:xs) = [y, x] ++ fliperoo xs

--Ex 6
sortuple :: (Ord a) => [(a,b)] -> [(a,b)]
sortuple [] = []
sortuple (x:xs) =   
    let smallerSorted = sortuple [a | a <- xs, fst a <= fst x]  
        biggerSorted = sortuple [a | a <- xs, fst a > fst x]  
    in  smallerSorted ++ [x] ++ biggerSorted

--Ex 7
--palindrome :: (Eq a) => [a] -> Bool
--palindrome x = x == invert x
palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome [x] = True
palindrome (x:xs) = if x /= last xs then False else palindrome (init xs)

--Ex 8
insert :: (Integral b) => [a] -> a -> b -> [a]
insert x y 0 = [y] ++ x
insert (x:xs) y i = [x] ++ insert xs y (i-1)

--Ex 9
size :: [a] -> Int
size [] = 0
size (x:xs) = 1 + size xs

--Ex 10
occurrences :: (Eq a) => [a] -> a -> Int
occurrences [] _ = 0
occurrences (x:xs) y = if x == y then 1 + occurrences xs y else occurrences xs y