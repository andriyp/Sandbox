-- JULKA

{-# LANGUAGE CPP, BangPatterns #-}
#define OPTS OPTIONS
-- {-# OPTS -O3 #-}
import Control.Monad

solve :: Integer -> Integer -> (Integer, Integer)
solve !n !k | odd k     = (klau + 1, nata)
            | otherwise = (klau,     nata)
  where
    !klau = (n `div` 2) + (k `div` 2)
    !nata = (n `div` 2) - (k `div` 2)

main = forM_ [1..10] $ \_ -> do
  !n <- readLn
  !k <- readLn
  let !(klau, nata) = solve n k
  print klau
  print nata