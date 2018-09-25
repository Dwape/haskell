--Ex 6
--Solve the 8 queens problem

boardSize = 8 --The board is of size boardSize*boardSize

data Position = Position {x :: Int, y :: Int} deriving Show

queens :: [[Position]]
queens = checkQueen []

--Returns all valid lists of positions
checkQueen :: [Position] -> [[Position]]
checkQueen [] = checkQueen [Position 0 0]
checkQueen (Position (-1) (-1):xs) = []
checkQueen (p:xs) = 
    let fails = check (p:xs) in 
    if (not fails) && (length xs == (boardSize-1)) then [p:xs] 
    else if fails then checkQueen (nextPosition p:xs)
    else checkQueen (nextRow p:p:xs) ++ checkQueen (nextPosition p:xs)

--Checks if a list of positions is valid
--Returns false when it is valid and true otherwise
check :: [Position] -> Bool
check [] = False
check (x:xs) = checkPosition x xs || check xs

--Compares a position to a list of positions
--Returns false when the position is valid and true otherwise
checkPosition :: Position -> [Position] -> Bool
checkPosition _ [] = False
checkPosition p1 (p2:xs) = (checkRows p1 p2) || (checkColumn p1 p2) || (checkDiagonal p1 p2) || (checkPosition p1 xs)

--Checks if two positions are in the same row
checkRows :: Position -> Position -> Bool
checkRows p1 p2 = x p1 == x p2

--Checks if two positions are in the same column
checkColumn :: Position -> Position -> Bool
checkColumn p1 p2 = y p1 == y p2

--Checks if two positions are in the same diagonal
checkDiagonal :: Position -> Position -> Bool
checkDiagonal p1 p2 = (x p1 - y p1) == (x p2 - y p2) || ((boardSize-1) - x p1 - y p1) == ((boardSize-1) - x p2 - y p2)

--Calculates the position that follows the position provided
--Returns Postition (-1) (-1) when the next position is not valid
nextPosition :: Position -> Position
nextPosition p
    | x p < (boardSize-1) = Position (x p + 1) (y p)
    | y p < (boardSize-1) = Position 0 (y p + 1)
    | otherwise = Position (-1) (-1)

--Returns the first position in the next row from the position provided
--Returns Postition (-1) (-1) when the next position is not valid
nextRow :: Position -> Position
nextRow p
    | y p < (boardSize-1) = Position 0 (y p + 1)
    | otherwise = Position (-1) (-1)

main :: IO()
main = do
    print (length queens)