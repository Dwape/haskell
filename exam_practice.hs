--Ex 1
data Tree a = Node a (Tree a) (Tree a) | Empty

instance Eq a => Eq (Tree a) where
    Empty == Empty = True
    (Node x l1 r1) == (Node y l2 r2) =  if x == y then (l1 == l2) && (r1 == r2) else False
    _ == _ = False

instance Show a => Show (Tree a) where --what is the best way of showing a tree?
    show (Node x l r) = show x ++ "(" ++ show l ++ "," ++ show r ++ ")"
    show Empty = show "null"

a = Node 1 (Node 2 Empty Empty) (Node 2 Empty Empty)
--b = Node 1 (Node 2 Empty Empty) (Empty)
--c = Node 1 Empty Empty
--d = Node 2 Empty Empty

--Ex 2
--type Graph = [(Int, [Int])]

--hasCyle :: Graph -> Bool
--hasCyle [] = False
--hasCyle (x:xs) = 
  --  let nodes = neighbours x in
  --  if (length nodes) == 0 then hasCyle xs
  --  else if (startingNode x (head nodes) [x] ((tail nodes) ++ (neighbours (head nodes)))) then True
  --  else hasCyle xs

--startingNode :: (Int, [Int]) -> (Int, [Int]) -> Graph -> Graph -> Bool
--startingNode _ _ _ [] = False
--startingNode starting actual visited (x:xs) = 
  --  if (value starting) == (value actual) then True 
  --  else if elem actual visited then False
  --  else (startingNode starting x (visited ++ [actual]) ((neighbours x) ++ xs) )

--value :: (Int, [Int]) -> Int
--value = fst

--neighbours :: (Int, [Int]) -> [Int]
--neighbours = snd

--a = [(1, [2])]
--b = [(2, [3])]
--c = [(3, [1])]
--graph = a ++  b ++ c

--Ex 3
data Bit = One | Zero
type Binary = [Bit]

instance Show Bit where
    show One = show 1
    show Zero = show 0

to_binary :: Int -> Binary
to_binary 0 = []
to_binary n = 
    case (quotRem n 2) of
        (n, 1) -> to_binary n ++ [One]
        (n, 0) -> to_binary n ++ [Zero]

to_binary' :: Int -> Binary
to_binary' 0 = []
to_binary' n
        | snd result == 1 = to_binary' (fst result) ++ [One]
        | snd result == 0 = to_binary' (fst result) ++ [Zero]
        where result = (quotRem n 2) 

build_tree :: Binary -> Tree Int
build_tree [] = Node 2 Empty Empty
build_tree (One:xs) = Node 1 Empty (build_tree xs)
build_tree (Zero:xs) = Node 0 (build_tree xs) Empty

traverse_tree :: Binary -> Tree a -> a
traverse_tree _ Empty = error "didn't work"
traverse_tree [] (Node a _ _) = a
traverse_tree (One:xs) (Node _ _ r) = traverse_tree xs r
traverse_tree (Zero:xs) (Node _ l _) = traverse_tree xs l

main :: IO()
main = do
    print(traverse_tree (to_binary' 12) build_tree(to_binary' 12))

