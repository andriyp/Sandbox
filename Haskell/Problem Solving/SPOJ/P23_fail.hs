{-# LANGUAGE CPP, BangPatterns #-}
#define OPTS OPTIONS
-- {-# OPTS -O3 #-}

import Numeric
import Control.Monad

type D = Double

volume :: D -> D -> D -> D -> D -> D -> D
volume  0  0  0  0   0   0  = 0
volume !u !w !v !w' !v' !u' = sqrt ( (b + c + d - a) * (a + c + d - b)
                                   * (a + b + d - c) * (a + b + c - d) )
                            / (192 * u * w * v)
  where
    a  = sqrt (x * y' * z')
    b  = sqrt (y * z' * x')
    c  = sqrt (z * x' * y')
    d  = sqrt (x * y * z)
    x' = (w - u' + v) * (u' + v + w)
    x  = (u' - v + w) * (v - w + u')
    
    y' = (u - v' + w) * (v' + w + u)
    y  = (v' - w + u) * (w - u + v')
    
    z' = (v - w' + u) * (w' + u + v)
    z  = (w' - u + v) * (u - v + w')

main = do n <- readLn
          putStrLn ""
          putStrLn ""
          forM_ [1..n] $ \_ -> do
            ln <- getLine
            putStrLn $ f $ map read (words ln)
  where
    f [!u, !w, !v, !w', !v', !u'] = showFFloat (Just 4) (volume u w v w' v' u') ""


test = putStrLn $ showFFloat (Just 4) (volume 1000 1000 1000 3 4 5) ""

{-
main = interact (unlines . map (f . map read . words) . tail . dropWhile (== "\n") . lines)
  where
    f [!u, !w, !v, !w', !v', !u'] = showFFloat (Just 4) (volume u w v w' v' u') ""
    f _                           = ""

test = putStrLn $ showFFloat (Just 4) (volume 0 0 0 0 0 0) ""
-}