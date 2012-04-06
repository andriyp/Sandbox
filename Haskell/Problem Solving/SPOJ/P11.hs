-- FCTRL

{-# LANGUAGE CPP, BangPatterns, MagicHash #-}
#define OPTS OPTIONS
-- {-# OPTS -O3 #-}

import Data.List

zeros :: Int -> Int
zeros n = go 0 n
  where
    go !a 0 = a 
    go !a n = go (a + k) k
      where 
        !k = n `div` 5

main = interact (unlines . map (show . zeros . read) . tail . lines)

{-
memo :: UArray Int Int
memo = listArray (0, 1000000) 
     $ concatMap (replicate 5) $ map (5^) [0..]
-}

-- import Data.Array.Unboxed
        
{-
import Data.Array.Unboxed

zeros :: Int -> Int
zeros n | n <= 10000 = memo ! n
        | otherwise  = k + zeros k
  where
    k = n `div` 5

memo :: UArray Int Int
memo = listArray (0,10000) xs
  where
    xs = 0 : [ k + xs !! k | i <- [1..] , let k = i `div` 5 ]
    
main = interact (unlines . map (show . zeros . read) . tail . lines)

log5 :: Int -> Int
log5 n | k == 0    = 0
       | otherwise = 1 + log5 k
  where
    k = n `div` 5

powSums5 = 1 : zipWith (+) powSums5 (map (5^) [1..])

memo :: UArray Int Int
memo = listArray (0, 12) (0 : powSums5)

zeros n = ((n - 5^k) `div` 5) + memo ! k
  where
    k = log5 n

--         0 : [ k + memo ! k | i <- [1..999]
--                            , let k = i `div` 5 ]
--  where
  --  xs = 0 : [ k + xs !! k | i <- [1..], let k = i `div` 5 ]

import GHC.Prim
import GHC.Exts

zeros :: Int -> Int
zeros (I# n) = I# (go n 0#)
  where
    go :: Int# -> Int# -> Int#
    go 0# !a = a
    go !n !a = go k (a +# k)
      where
        !k = n `quotInt#` 5#
-}

-- zeros = zLogs

-- array (0,12) ((0, 1) : [ (k, 5^k + zLogs ! (k-1)) | k <- [1..12] ])
    
-- main = interact (unlines . map (show . zeros . read) . tail . lines)

{-
facs = 1 : 1 : zipWith (*) [2..] (tail facs)

zeros = length . takeWhile ((== 0) . (`mod` 10)) . iterate (`div` 10)

test l u = forM_ [l..u] $ \n -> do
  let fac = facs !! n
  print (n, zeros fac)

pairwise f (x:xs@(x':_)) = f x x' : pairwise f xs
pairwise _ _             = []

grp []     = []
grp (x:xs) = x : grp (dropWhile (== x) xs)

infixr 8 .:
(.:) f g x y = f (g x y)

diffs l u = maximum $ pairwise (abs .: (-))
          $ grp $ map zeros $ take (u-l)
          $ drop l facs
-}