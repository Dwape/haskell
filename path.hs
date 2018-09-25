data Node' a = Node' a [Node' a] deriving Show
type Graph a = [Node' a]

path_exists :: Graph a -> Node' a -> Node' a -> Bool
path_exists [] _ _ = False
