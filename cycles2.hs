--Ex 3
-- |A Graph implementation which can check if there are cycles in the Graph
-- Graphs are considered directed
-- If a node is connected to itself, there is a cycle
data Node a = Node a [Node a] deriving Show
type Graph a = [Node a]

-- Returns true is the Graph provided has a cycle, false otherwise
cycles :: (Eq a) => Graph a -> Bool
cycles [] = False
cycles g = 
    let result = deadEnds g in
    if null result then True
    else cycles (removeAll (filterGraph g) result)

-- Returns a list with all the dead ends in a Graph
-- A dead end is a node from which you can get to no other nodes
deadEnds :: (Eq a) => Graph a -> [Node a]
deadEnds [] = []
deadEnds ((Node a []):g) = [(Node a [])] ++ deadEnds g
deadEnds (n:g) = deadEnds g

-- Removes all dead ends from the Graph provided
filterGraph :: (Eq a) => Graph a -> Graph a
filterGraph [] = []
filterGraph ((Node a []):g) = filterGraph g
filterGraph (n:g) = [n] ++ filterGraph g

removeAll :: (Eq a) => Graph a -> [Node a] -> Graph a
removeAll g [] = g
removeAll g (n:xs) = removeAll (remove n g) xs 

remove :: (Eq a) => Node a -> Graph a -> Graph a
remove n [] = []
remove n ((Node a x):xs) = [Node a (remove2 x n)] ++ remove n xs

remove2 :: (Eq a) => [Node a] -> Node a -> [Node a]
remove2 [] _ = []
remove2 ((Node b l):xs) (Node a c) = 
    if a == b then xs 
    else [(Node b l)] ++ remove2 xs (Node a c)

--newRemove :: (Eq a) => [Node a] -> Node a -> Node a
--newRemove [] _ = []
--newRemove ((Node b l):xs) (Node a c) = 
--    if a == b then xs 
--    else [(Node b l)] ++ newRemove xs (Node a c)

--removeAll :: (Eq a) => Graph a -> [Node a] -> Graph a
--removeAll g [] = g
--removeAll g (x:xs) = removeAll (map (newRemove (x:xs)) g) xs

a = Node 1 [b, c]
b = Node 2 [c]
c = Node 3 []
d = Node 4 [e]
e = Node 5 []

main :: IO()
main = do
    --print (cycles [Node 3 [], Node 2 [Node 3 []], Node 1 [Node 2 []]])
    --print (cycles [Node 3 [Node 1 []], Node 2 [Node 3 []], Node 1 [Node 2 []]])
    print (cycles [a, b, c, d, e])