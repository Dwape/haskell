--1
last' :: [a] -> a
last' [] = error "no element"
last' [a] = a
last' (_:xs) = last' xs

--2
last_but_one :: [a] -> a
last_but_one [] = error "no such element"
last_but_one [a] = error "no such element"
last_but_one [a, b] = a
last_but_one (x:xs) = last_but_one xs

--3
nth :: [a] -> Int -> a
nth [] _ = error "no such element"
nth (x:xs) 1 = x
nth (x:xs) n = nth xs (n-1)

--4
size :: [a] -> Int
size [] = 0
size (x:xs) = 1 + size xs

--4 with tail recursion
size' :: [a] -> Int
size' l = size2 l 0

size2 :: [a] -> Int -> Int
size2 [] n = n
size2 (x:xs) n = size2 xs (n+1)

myLength :: [a] -> Int
myLength =  foldl (\n _ -> n + 1) 0
--myLenght l = foldl (\n _ -> n + 1) 0 l --dose the same thing but one can be read

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--6
palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome [a] = True
palindrome (x:xs) = if x == last xs then palindrome (init xs) else False

--7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

--8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:xs) = if x == head xs then compress xs else x:compress xs

--9
pack :: (Eq a) => [a] -> [[a]]
pack l = pack' l []

pack' :: (Eq a) => [a] -> [a] -> [[a]]
pack' [a] l = [a:l]
pack' (x:xs) l = if x == head xs then pack' xs (x:l) else (x:l):pack' xs []

--10
encode :: (Eq a) => [a] -> [(Int, a)]
encode l = map (\x -> (length x, head x)) (pack l)

--11
data ListItem a = Single a | Multiple Int a deriving (Show)
encode' :: (Eq a) => [a] -> [ListItem a]
--encode' l = [(Multiple (length x) (head x))| x <- (pack l)]
encode' l = [if (length x) == 1 then (Single (head x)) else (Multiple (length x) (head x))| x <- (pack l)]

--12
decode' :: (Eq a) => [ListItem a] -> [a]
decode' [] = []
decode' ((Single x):xs) = x:decode' xs
decode' ((Multiple n x):xs) = (take n (repeat x)) ++ decode' xs

--13
direct_encode :: (Eq a) => [a] -> [a] -> [ListItem a]
direct_encode [a] [] = [(Single a)]
direct_encode [a] l = [(Multiple ((length l)+1) a)]
direct_encode (x:xs) l = if x == head xs then direct_encode xs (x:l) else if (length l) == 0 then (Single x):direct_encode xs [] else(Multiple ((length l)+1) x):direct_encode xs []

--14
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x:duplicate xs

--15
multiply :: [a] -> Int -> [a]
multiply [] _ = []
multiply (x:xs) n = (take n (repeat x)) ++ multiply xs n

--16
drop_nth :: [a] -> Int -> [a]
drop_nth l n = drop_nth' l n n

drop_nth' :: [a] -> Int -> Int -> [a]
drop_nth' [] _ _ = []
drop_nth' (x:xs) n 1 = drop_nth' xs n n
drop_nth' (x:xs) n acc = x:drop_nth' xs n (acc-1)

--17
split :: [a] -> Int -> ([a], [a])
split l n = split' l n []

split' :: [a] -> Int -> [a] -> ([a], [a])
split' [] _ _ = ([], [])
split' l1 0 l2 = (l2, l1)
split' (x:xs) n l = split' xs (n-1) (l ++ [x])

--18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) 1 0 = []
slice (x:xs) 1 n = x:slice xs 1 (n-1)
slice (x:xs) n m = slice xs (n-1) (m-1)

--19
rotate :: [a] -> Int -> [a]
rotate l n = drop n l ++ take n l

--20
remove :: [a] -> Int -> (a, [a])
remove (x:xs) 0 = (x, xs)

--main
main :: IO()
main = do
    print( rotate [1 ,2 ,3, 4, 5, 6, 7] (-3))
