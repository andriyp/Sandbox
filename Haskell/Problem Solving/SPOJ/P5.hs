{-# LANGUAGE CPP #-}
#define OPTS OPTIONS
{-# OPTS -O3 #-}
import Data.List
import Data.Maybe
import Control.Monad

divide s | even len  = (l, [], r)
         | otherwise = (l, [head r], tail r)
  where 
    len   = length s
    (l,r) = splitAt (len `div` 2) s

strRevSucc []       = []
strRevSucc ('9':cs) = '0' : strRevSucc cs
strRevSucc ( c :cs) = succ c : cs

mismatch []     _                  = Nothing
mismatch _      []                 = Nothing
mismatch (x:xs) (y:ys) | x == y    = mismatch xs ys
                       | otherwise = Just (x, y)

nextPalin s | isNothing mbNeq || 
              uncurry (<) (fromJust mbNeq) = if all (== '0') rwr 
                                               then '1' : (tail rwr ++ "1") 
                                               else rwr
            | otherwise = l ++ (m ++ lr)
  where
    (l, m, r) = divide s
    lr        = reverse l
    mbNeq     = mismatch lr r
    lr'       = strRevSucc lr
    m'        = strRevSucc m
    
    rwr | m == "" || m == "9" = reverse lr' ++ (m' ++ lr')
        | otherwise           = l           ++ (m' ++ lr)

main = do n <- readLn
          forM_ [1..n] $ \_ -> do
            ln <- getLine
            putStrLn (nextPalin ln)

-- main = interact (unlines . map nextPalin . tail . lines)