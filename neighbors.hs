--Ex 4
--Determine if a path exists between two nodes of a Graph
data Node a = Node a [Node a] deriving Show
type Graph a = [Node a]