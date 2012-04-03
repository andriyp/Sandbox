-- ADDREV

{-# LANGUAGE CPP #-}
#define OPTS OPTIONS
-- {-# OPTS -O3 #-}
import Data.Function

infixr 8 .:
(.:) f g x y = f (g x y)

addrev = dropWhile (== '0') . reverse . show .: (+) `on` (read . reverse)

main = interact (unlines . map ((\[x,y] -> addrev x y) . words) . tail . lines)