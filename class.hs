--Functor (Funtori)
--fmap (fmapi)
class Functori f where
    fmapi :: (a -> b) -> f a -> f b

instance Functori [] where
    fmapi = map

instance Functori Maybe where
    fmapi f (Just x) = Just (f x)
    fmapi f Nothing = Nothing

partialFunctions = map (*) [1, 2, 3]
result = map (\x -> x 3) partialFunctions

main :: IO()
main = do
    print (map (fmapi (\x -> x+1)) [Just 1, Just 2, Nothing, Just 3])
    print (result)
    print (map (\f -> f 3) $ map (+) [1, 2, 3])