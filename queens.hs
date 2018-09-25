--Ex 6
--Solve the 8 queens problem

boardSize = 8 --The board is of size boardSize*boardSize

data Position = Position {x :: Int, y :: Int} deriving Show

queens :: [[Position]]
queens = checkQueen []

--Checks for a specific first queen position.
--Returns all results for that first queen.
checkQueen :: [Position] -> [[Position]]
checkQueen [] = checkQueen [Position 0 0]
checkQueen (Position (-1) (-1):xs) = []
checkQueen (p:xs) = 
    let fails = check (p:xs) in 
    if (not fails) && (length xs == (boardSize-1)) then [p:xs] 
    else if fails then checkQueen (nextPosition p:xs)
    else checkQueen (nextPosition p:p:xs) ++ checkQueen (nextPosition p:xs)

--Maybe for a full length solution we could check if all rows and columns are different.
--This will not be that useful in this solution as a small part of the calls to this function are done with full length solutions as arguments.
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
checkDiagonal p1 p2 = (x p1 - y p1) == (x p2 - y p2) || ((boardSize-1) - x p1 - y p1) == ((boardSize-1) - x p2 - y p2)

nextPosition :: Position -> Position
nextPosition p
    | x p < (boardSize-1) = Position (x p + 1) (y p)
    | y p < (boardSize-1) = Position 0 (y p + 1)
    | otherwise = Position (-1) (-1)

--Check for every first queen
main :: IO()
main = do
    print (length queens)
