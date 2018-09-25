doubleMe :: Int -> Int
doubleMe a = a*2

sort :: [Int] -> [Int]
sort xs = [x| x<-xs]

get :: [a] -> Int -> a
get [] _ = error("You fucked up")
get (x:xs) 0 = x
get (x:xs) i = get xs (i-1)

--max :: (Ord a) => [a] -> a

bigger :: (Ord a) => a -> a -> a
bigger x y = if x > y then x else y

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * (factorial (x-1))

something :: Int -> Char
something x
    |x <= 0 = 'a'
    |x <= 10 = 'b'
    |x <= 20 = 'c'
    |otherwise = 'd'

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2 

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea

head :: [a] -> a
head xs = 
	case xs of 
		[] = error "Nice"
		(x:_) = x


