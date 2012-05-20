{-# LANGUAGE TypeOperators #-}

data a :. b = a :. b deriving (Eq, Show)

(<^>) :: (s1 -> i1 -> (s1, o1)) -> (s2 -> i2 -> (s2, o2)) -> (s1 :. s2) -> (i1 :. i2) -> (s1 :. s2, o1 :. o2)
(<^>) f g ~(s1 :. s2) ~(i1 :. i2) = (s1' :. s2', o1 :. o2)
  where
    ~(s1', o1) = f s1 i1
    ~(s2', o2) = g s2 i2

moo s x = (s+1, x-1)

test = (moo <^> moo <^> moo) (1 :. 2:. 3) (5 :. 5 :. 5) == (2 :. 3 :. 4, 4 :. 4 :. 4)