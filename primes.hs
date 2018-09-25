--Ex 5
--Create a function that returns the first n prime numbers
primes :: (Integral a) => a -> [a]
primes 0 = []
primes x =  primesP x 1 []

primesP :: (Integral a) => a -> a -> [a] -> [a]
primesP 0 _ l = []
primesP n p l = 
    let q = nextPrime p l in [q] ++ primesP (n-1) q ([q] ++ l)

nextPrime :: (Integral a) => a -> [a] -> a
nextPrime 1 _ = 2
nextPrime x l = if isPrime (x+1) l then (x+1) else nextPrime (x+1) l

--We could pass the list of smaller primes as parameter to make the verification more efficient
isPrime :: (Integral a) => a -> [a] -> Bool
isPrime _ [] = True
isPrime x (h:ls) = if rem x h == 0 then False else isPrime x (ls)

primes2 :: (Integral a) => Int -> [a]
primes2 n = take n [i | i<-[2..], isPrime2 i (round(sqrt (fromIntegral i)))]

isPrime2 :: (Integral a) => a -> a -> Bool
isPrime2 _ 1 = True
isPrime2 x n = if rem x n == 0 then False else isPrime2 x (n-1)

--Try it with the list being in reverse order
--Square root of the number (with list comprehension over the list)

main :: IO()
main = do
    print (primes2 1000)
