--Ex 7
--Huffman compression algorith
data HuffmanTree = CharNode Int Char HuffmanTree HuffmanTree | Node Int HuffmanTree HuffmanTree | Empty deriving Show

--Encodes a string using the Huffman algorithm
encode :: String -> String
encode s = encodeString s (createCodes (buildTree (sortedCharNodes (frequency s))) [])

--Returns the an encoded string using the codes provided
encodeString :: String -> [(String, Char)] -> String
encodeString [] _ = []
encodeString (x:xs) l = code x l ++ encodeString xs l

--Returns the code for a given char
code :: Char -> [(String, Char)] -> String
code x ((s, c):xs) = if x == c then s else code x xs

--Returns a list of tuples with each char and its Huffman code
createCodes :: HuffmanTree -> String -> [(String, Char)]
createCodes (Node _ l r) s = createCodes l (s ++ ['0']) ++ createCodes r (s ++ ['1'])
createCodes (CharNode _ c _ _) s = [(s, c)]

--Returns a Huffman tree composed of all the nodes passed as function parameters.
buildTree :: [HuffmanTree] -> HuffmanTree
buildTree [] = Empty
buildTree [t] = t
buildTree (x:y:t) = 
    let merged = merge x y in
    buildTree (insertNode merged t)

--Combines two trees into one
--The root of this new tree has a frequency equal to the sum of the frequency of its two children
merge :: HuffmanTree -> HuffmanTree -> HuffmanTree
merge x y = Node (freq x + freq y) x y

--Inserts a huffman tree into a huffman tree list in order
insertNode :: HuffmanTree -> [HuffmanTree] -> [HuffmanTree]
insertNode x [] = [x]
insertNode x (r:t) = 
    if freq x <= freq r
        then [x] ++ (r:t) 
        else [r] ++ insertNode x t

--Returns the frquency of a node
freq :: HuffmanTree -> Int
freq (CharNode n _ _ _) = n
freq (Node n _ _) = n

--Returns a list of huffman tree nodes with characters and their frequency in the string, sorted by their frequency (in ascending order)
sortedCharNodes :: [(Int, Char)] -> [HuffmanTree]
sortedCharNodes [] = []
sortedCharNodes ((n, c):xs) = 
    let smallerSorted = sortedCharNodes [(a, b) | (a, b) <- xs, a <= n]  
        biggerSorted = sortedCharNodes [(a, b) | (a, b) <- xs, a > n]  
        in smallerSorted ++ [CharNode n c Empty Empty] ++ biggerSorted

--Returns a list of tuples with the frequency of each character in the string
frequency :: String -> [(Int, Char)]
frequency [] = []
frequency (x:xs) = 
    let size = length (x:xs) 
        filteredString = filterChar (x:xs) x 
        in [(size - length filteredString, x)] ++ frequency filteredString

--Removes all instances of a character in a string
filterChar :: String -> Char -> String
filterChar [] _ = []
filterChar (x:xs) c = if x == c then filterChar xs c else [x] ++ filterChar xs c

main :: IO()
main = do
    print (encode "Hello, World!")   