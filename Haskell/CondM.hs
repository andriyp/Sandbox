

{-
data C a = C a (a -> Bool)

instance Monad C where
  
  return x = C x (const True)

  (C x p) >>= f | p x       = f x
                | otherwise = return x
-}

newtype S a r = S { runS :: a -> (a , r) }

instance Monad (S a) where
  return x = S (\a -> (a, x))
  st >>= f = S (\a -> let (a',r) = runS st a in runS (f r) a')