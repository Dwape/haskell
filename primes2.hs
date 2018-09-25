--Ex 5
--Create a function that returns the first n prime numbers
primes :: (Integral a) => a -> [a]
primes 0 = []
primes x =  primesP x 1 []

primesP :: (Integral a) => a -> a -> [a] -> [a]
primesP 0 _ l = []
primesP n p l = 
    let q = nextPrime p [ i | i<-l, i<round(sqrt (fromIntegral (head l)))] in [q] ++ primesP (n-1) q ([q] ++ l)

nextPrime :: (Integral a) => a -> [a] -> a
nextPrime 1 _ = 2
nextPrime x l = if isPrime (x+1) l then (x+1) else nextPrime (x+1) l

isPrime :: (Integral a) => a -> [a] -> Bool
isPrime _ [] = True
isPrime x (h:ls) = if rem x h == 0 then False else isPrime x (ls)

main :: IO()
main = do
    print (primes 1000)
