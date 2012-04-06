-- PCode : PRIME1

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS_GHC -O3 #-}

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef

fi = fromIntegral

var = newSTRef
val = readSTRef

(<-.#) a v i = writeArray a i v
(<-$) = modifySTRef
(?#)  = readArray
(?$)  v p = fmap p (val v)

newSTUA :: (MArray (STUArray s) e (ST s), Ix i) 
        => (i,i) -> e -> ST s (STUArray s i e)           
newSTUA b e = newArray b e

eratoSieve n = runST $ do
  
  let u = ceiling $ sqrt (fi n)
      
  sieve <- newSTUA (2, n) True
  
  forM_ [2 .. u] $ \p -> do    
    x <- readArray sieve p
    when x $
      forM_ [p^2, p^2+p .. n] $ \i ->
        writeArray sieve i False
  
  r <- var [1]
    
  forM_ [2..n] $ \i -> do
    x <- sieve ?# i
    when x (r <-$ (i:))
  
  r <-$ reverse
  val r

main = print $ last (eratoSieve 1000000)

{-
main = interact ( primes . tail . lines)
  where
    primes xs = 1
      where map roRange xs
-}

{-

while pm m = pm >>= \b -> when b (m >> while pm m)

eratoSieve n = runST $ do
  
  let u = ceiling $ sqrt (fi n)
      
  sieve <- newSTA (2, n) True
  p     <- var 2
  
  while (p ?$ (< u)) $ do
    i <- val p
    x <- sieve ?# i
    
    when x $ do
      forM_ [i^2, i^2+i .. n] $
        sieve <-.# False
    
    p <-$ succ
  
  r <- var []
  forM_ [2..n] $ \i -> do
    x <- sieve ?# i
    when x (r <-$ (i:))
  
  r <-$ reverse
  val r



(?$#) a p = fmap p . readArray a

eratoSieve :: Integral a => a -> [a]
eratoSieve n = runST $ do
  
  sieve <- newArray (2, n) True :: ST s (STArray s a Bool)
  p     <- var 2
  
  let u = ceiling $ sqrt (fi n)
  
  while (p ?$ (< u)) $ do
    i <- val p
    
    for [i^2, i^2+i .. u] $
      sieve <.# False
      
    while ((sieve ?$# not) =<< p) $
      p <$ succ
    
  forM_ [2..n] $ \i -> do
    
  a <- readArray arr 1
                writeArray arr 1 64
                b <- readArray arr 1
                return (a,b)
-}