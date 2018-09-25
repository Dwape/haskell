--bigger :: (Ord a) => a -> (a -> Bool)
--bigger x y = x > y

--smaller4 :: (Ord a) => a -> Bool
--smaller4 = bigger 4

--map :: (a -> b) -> [a] -> [b]

is_even :: (Integral a) => a -> Bool
is_even x = mod x 2 == 0

data Gianni = Late | Does_not_show_up deriving (Show)

time :: (Ord a, Num a) => a -> Gianni
time x = if x > 12 then Late else Does_not_show_up

data Result a = Null | Only a deriving (Show)

divide :: (RealFloat a) => a -> a -> Result a
divide _ 0 = Null
divide a b = Only (a/b)

unpack :: Result a -> a
unpack Null = error "Error"
unpack (Only a) = a

class Dude d where
    zupp ::  d -> d

instance Dude Gianni where
    zupp Late = Does_not_show_up
    zupp Does_not_show_up = Late 

main :: IO()
main = do
    print (zupp Does_not_show_up)