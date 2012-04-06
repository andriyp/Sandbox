{-# LANGUAGE ViewPatterns #-}
-- PCode : PRIME1

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

(&&&) f g x = (f x, g x)

-- solver for biquadratic integral forms of kind ax^2 + by^2 = n
-- returns list of integral solutions (x, y)
biQuadSolve :: (Integral a) => a -> a -> a -> [(a, a)]
biQuadSolve a b n = [ (x, y) | x <- [xMin..xMax]
                             , let y' = sqrt $ fi (n - a * x^2) / fi b
                             , let y  = truncate y'
                             , fi y == y'
                             ]
  where    
    xMin = ceiling $ sqrt $ fi (n - b) / fi a
    xMax = floor   $ sqrt $ fi (n - 1) / fi a

atkinSieve :: Integral a => a -> [a]
atkinSieve n = sieve
             $ map (flipIf (`elem` sieveList))
                   [ (False, i) | i <- [7..n] ]
  where
    sieveList = concatMap flipListFor [1..n]
    
    sieve [] = []
    sieve xs = y : sieve (map (flipIf (`multiple` y)) ys)
      where
        ((_, y) : ys) = dropWhile (not . fst) xs
    
    multiple x y = (x `mod` y) == 0
    
    flipIf pred t@(isPrime, x)
      | pred x    = (not isPrime, x)
      | otherwise = t
                
    flipListFor n@((`mod` 60) -> r)
      | r `elem` [1, 13, 17, 29, 37, 41, 49, 53] = biQuadSolve 4 1 n
      | r `elem` [7, 19, 31, 43]                 = biQuadSolve 3 1 n
      | r `elem` [11, 23, 47, 59]                = filter (uncurry (>))
                                                 $ biQuadSolve 3 (-1) n
      | otherwise                                = []