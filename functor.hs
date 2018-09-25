class functor f where
fmap :: (a -> b) -> f a -> f b

instance functor Maybe where
    fmap _ Nothing = Nothing
    fmap f Just a = Just (f a)

class (functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

--An Applicative can be a type or a function
--If f is a function, it will apply the function to the argument (in this case something)
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something --f is a function

--The variable type is a function
z :: Maybe (a -> b) -> c

