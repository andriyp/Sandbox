{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}

import qualified Data.Map.Lazy as M
import System.IO
import Control.Arrow
import Data.List
import GHC.Exts
import Data.Ord
import Control.Parallel.Strategies
import Data.Maybe

fi = fromIntegral

minBySnd = minimumBy (comparing snd)

listToTup3 [a,b,c] = (a,b,c)

readCoord = listToTup3 . map read . words

type Pt3 a = (a,a,a)

class MetricSpace a where
  distance :: a -> a -> Float

instance MetricSpace (Pt3 Int) where
  distance (x1,y1,z1) (x2,y2,z2)
    = sqrt $ (fi x1 - fi x2)^2 + (fi y1 - fi y2)^2 + (fi z1 - fi z2)^2

minInnerDistance pts@(_:_:_) = Just $ minBySnd [ ((p1,p2), distance p1 p2)
                                               | p1 <- pts, p2 <- pts, p1 /= p2 ]
minInnerDistance _           = Nothing

minOuterDistance m q = minBySnd [ ((pt,nb), distance pt nb)
                                | nb <- concat $ catMaybes $ map (`M.lookup` m) $ neighbours q
                                , pt <- fromJust $ M.lookup q m ]

qCoord d (x,y,z) = (x `div` d, y `div` d, z `div` d)

neighbours (x,y,z) = [ (x, y,   z+1), (x-1, y,   z+1), (x+1, y,   z+1)
                     , (x, y-1, z+1), (x-1, y-1, z+1), (x+1, y-1, z+1)
                     , (x, y+1, z+1), (x-1, y+1, z+1), (x+1, y+1, z+1)
                                                       
                     , (x, y,   z-1), (x-1, y,   z-1), (x+1, y,   z-1)
                     , (x, y-1, z-1), (x-1, y-1, z-1), (x+1, y-1, z-1)
                     , (x, y+1, z-1), (x-1, y+1, z-1), (x+1, y+1, z-1)
                     
                     , (x-1, y, z), (x+1, y,   z)
                     , (x, y+1, z), (x-1, y+1, z), (x+1, y+1, z)
                     , (x, y-1, z), (x-1, y-1, z), (x+1, y-1, z)
                     ]

calcMinDist pts = minBySnd (rmin ++ cmin)
  where
    m = M.fromList
      $ map ((fst . head) &&& map snd)
      $ groupWith fst
      $ map (qCoord 50000 &&& id) pts
    rmin = catMaybes $ parMap rdeepseq minInnerDistance (M.elems m)
    cmin = parMap rdeepseq (minOuterDistance m) (M.keys m)

main = withFile "S:/prog/sandbox/Haskell/Problem Solving/FPFP/GoldilocksBelt/stars.dat" ReadMode $ \file -> do
  contents <- hGetContents file
  print $ calcMinDist $ map (readCoord :: String -> Pt3 Int) (lines contents)