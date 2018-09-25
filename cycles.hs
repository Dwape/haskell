--Ex 3
--Implement Graph and check if there are cycles
data Node a = Node a [Node a] deriving Show
type Graph a = [Node a]

cycles :: (Eq a) => Graph a -> Bool
cycles [] = False
cycles g = 
    let result = removeAll (filterEmpty g) (check g) in
    if length g == length result then True
    else cycles result

removeAll :: (Eq a) => Graph a -> [Node a] -> Graph a
removeAll g [] = g
removeAll g (n:xs) = removeAll (remove n g) xs 

remove :: (Eq a) => Node a -> Graph a -> Graph a
remove n [] = []
remove n ((Node a x):xs) = [Node a (remove2 n x)] ++ remove n xs

remove2 :: (Eq a) => Node a -> [Node a] -> [Node a]
remove2 _ [] = []
remove2 (Node a c) ((Node b l):xs) = 
    if a == b then xs 
    else [(Node b l)] ++ remove2 (Node a c) xs

check :: (Eq a) => Graph a -> [Node a]
check [] = []
check ((Node a []):g) = [(Node a [])] ++ check g
check (n:g) = check g

filterEmpty :: (Eq a) => Graph a -> Graph a
filterEmpty [] = []
filterEmpty ((Node a []):g) = filterEmpty g
filterEmpty (n:g) = [n] ++ filterEmpty g

a = Node 1 [b]
b = Node 2 [c]
c = Node 3 []
d = Node 4 [e]
e = Node 5 [d]

main :: IO()
main = do
    --print (cycles [Node 3 [], Node 2 [Node 3 []], Node 1 [Node 2 []]])
    --print (cycles [Node 3 [Node 1 []], Node 2 [Node 3 []], Node 1 [Node 2 []]])
    print (cycles [a, b, c, d, e])

