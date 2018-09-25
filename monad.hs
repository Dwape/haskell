--For assosiative types
class Monoid m where
	mempty :: m
	mappend :: m -> m -> m
	mconcat :: [m] -> m
	mconcat = foldr mappend mempty

class Monad m where
	return :: a -> m a --Returns the contextualized a
	(>>=) :: m a -> (a -> m b) -> m b
	(>>) :: m a -> m b -> m b
	x >> y = x >>= \_ -> y
	fail :: String -> m a
	fail msg = error msg

instance Mondad Maybe where
	return x = Just x
	Nothing >>= f = Nothing
	Just x >>= f = fx
	fail _ = Nothing