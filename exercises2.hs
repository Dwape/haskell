--Ex 1
--Binary sum
-- 1 Implementar la suma binaria
data Bit = Zero | One deriving Show
type Binary = [Bit]

binarySum :: Bit-> Binary -> Binary -> Binary
binarySum One [] [] = [One]
binarySum _ [] [] = []
binarySum z [] (y:ys) = 
    let result = bitsum Zero y z
    in [fst result] ++ binarySum (snd result) [] ys
binarySum z (y:ys) [] = 
    let result = bitsum Zero y z
    in [fst result] ++ binarySum (snd result) [] ys
binarySum z (x:xs) (y:ys) = 
    let result = bitsum x y z 
    in [fst result] ++ binarySum (snd result) xs ys

bitsum :: Bit -> Bit -> Bit -> (Bit, Bit)
bitsum Zero Zero Zero = (Zero, Zero)
bitsum One Zero One = (Zero, One)
bitsum One One Zero = (Zero, One)
bitsum Zero One One = (Zero, One)
bitsum One One One = (One, One)
bitsum _ _ _ = (One, Zero)

--Ex 2
--Define a tree and a function to construct it
--TODO add method for tree
data Tree a = Node a (Tree a) (Tree a) | Empty deriving Show

plantTree :: [a] -> Tree a
plantTree [] = Empty
plantTree (x:xs) = Node x (plantTree (take (quot(length xs) 2) xs)) (plantTree (drop (quot(length xs) 2) xs))

--Ex 3
--Implement Graph and check if there are cycles
data Node' a = Node' a [Node' a] deriving Show
type Graph a = [Node' a]

--cycles :: Graph -> Graph -> Bool
--cycles

simplify :: Graph a -> Graph a
simplify x = [ i | i <- x, hasNeighbors i]

hasNeighbors :: Node' a -> Bool
hasNeighbors (Node' x []) = False
hasNeighbors _ = True

--Ex 4
--Determine if a path exists between two nodes of a Graph

--Ex 5
--Create a function that returns the first n prime numbers
primes :: (Integral a) => a -> [a]
primes 0 = []
primes x =  primesP x 1 

primesP :: (Integral a) => a -> a -> [a]
primesP 0 _ = []
primesP n p = 
    let q = nextPrime p in [q] ++ primesP (n-1) q

nextPrime :: (Integral a) => a -> a
nextPrime 1 = 2
nextPrime x = if isPrime (x+1) x then (x+1) else nextPrime (x+1)

--We could pass the list of smaller primes as parameter to make the verification more efficient
isPrime :: (Integral a) => a -> a -> Bool
isPrime _ 1 = True
isPrime x n = if rem x n == 0 then False else isPrime x (n-1)

--Ex 6
--Solve the 8 queens problem
data Position = Position {x :: Int, y :: Int} deriving Show

queens :: [[Position]] -> [[Position]]
queens (x:xs) = 
    let ls = queens xs in if check x then ls else x : ls

check :: [Position] -> Bool
check [] = False
check (x:xs) = checkPosition x xs || check xs

checkPosition :: Position -> [Position] -> Bool
checkPosition _ [] = False
checkPosition p1 (p2:xs) = (checkRows p1 p2) || (checkColumn p1 p2) || (checkDiagonal p1 p2) || (checkPosition p1 xs)

checkRows :: Position -> Position -> Bool
checkRows p1 p2 = x p1 == x p2

checkColumn :: Position -> Position -> Bool
checkColumn p1 p2 = y p1 == y p2

checkDiagonal :: Position -> Position -> Bool
checkDiagonal p1 p2 = (x p1 - y p1) == (x p2 - y p2) || (7 - x p1 - y p1) == (7 - x p2 - y p2)

generator :: [[Position]]
generator = let ls = [ Position i j | i <- [0..7], j <- [0..7]] in [[t0, t1, t2, t3, t4, t5, t6, t7] | t0<-ls, t1<-ls, t2<-ls, t3<-ls, t4<-ls, t5<-ls, t6<-ls, t7<-ls]

--Ex 7
--Huffman compression algorith

--Ex 8
--Determine the depth of a tree
treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node root left right) = 1 + max (treeDepth left) (treeDepth right)

--Ex 9
--Implement a contact list
data Contact = Contact { name :: String, phone :: Int, email :: String } deriving Show
type ContactList = [Contact]

addContact :: ContactList -> String -> Int -> String -> ContactList
addContact list name phone email = list ++ [Contact name phone email]

searchName :: ContactList -> String -> Contact
searchName [] n = error ("No contact found with name " ++ n)
searchName (x:xs) n = if name x == n then x else searchName xs n

searchEmail :: ContactList -> String -> Contact
searchEmail [] e = error ("No contact found with email " ++ e)
searchEmail (x:xs) e = if name x == e then x else searchEmail xs e

--Ex 10
--Transpose a matrix
transpose :: [[a]] -> [[a]]
transpose ([]:xs) = []
transpose x = (map head x) : (transpose (map tail x))

dud :: String
dud = let f = generator in "done"

main :: IO()
main = do
    print (dud)
