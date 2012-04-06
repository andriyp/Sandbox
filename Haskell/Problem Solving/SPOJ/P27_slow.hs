-- SBANK

{-# LANGUAGE CPP #-}
#define OPTS OPTIONS
-- {-# OPTS -O3 #-}
import Data.List
import Control.Monad

count []     = []
count (x:xs) = go x 1 xs
  where
    go _ _ []                 = []
    go x k (y:ys) | x == y    = go x (k+1) ys
                  | otherwise = (x,k) : go y 1 ys

main = do t <- readLn
          forM_ [1..t] $ \_ -> do
            n <- readLn
            accs <- sequence (replicate n getLine)
            mapM_ prn (count (sort accs))
            getLine
            putStrLn ""
  where
    prn (acc,k) = putStrLn (acc ++ " " ++ show k)