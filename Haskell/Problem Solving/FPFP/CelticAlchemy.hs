{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Ord
import Data.List
import Data.Char
import Data.Maybe
import GHC.Exts
import System.Environment
import System.IO
import Control.Monad
import Control.Applicative
import Data.Graph.Inductive

-- | Utilities

(.:) f g x y = f (g x y)

chunksOf _ [] = []
chunksOf n xs = l : chunksOf n r
  where
    (l, r) = splitAt n xs

nths ns xs = map (xs !!) ns

-- | Tries

data TrieMap k v = Leaf v
                 | Trie (Map k (TrieMap k v))
                 
                 deriving ( Eq, Show )

catLeaves []            = []
catLeaves (Leaf x : xs) = x : catLeaves xs
catLeaves (_      : xs) = catLeaves xs

trieMapEmpty = Trie M.empty

trieMapSingleton ks v = foldr (Trie .: M.singleton) (Leaf v) ks

trieMapInsert (k:ks) v (Trie m) = reconsKth 
                                $ maybe (trieMapSingleton ks v)
                                        (trieMapInsert ks v)
                                        (M.lookup k m)
  where
    reconsKth new = Trie (M.insert k new m)

trieMapInsert ks v _ = trieMapSingleton ks v

trieMapLookup []     t        = Just t
trieMapLookup (k:ks) (Trie m) = join $ trieMapLookup ks <$> M.lookup k m
trieMapLookup _      _        = Nothing

--

findNeighbors []     _          = []
findNeighbors (k:ks) t@(Trie m) = xs ++ ys
  where
    t' = M.lookup k m
    xs = catLeaves $ catMaybes 
       $ map (flip trieMapLookup t . (:ks))
       $ delete k (M.keys m)
    ys = maybe [] (findNeighbors ks) t'

buildTrieMapAndGraph words = go 0 words trieMapEmpty [] []
  where
    go _ []     t nodes edges = (t, mkUGraph nodes edges :: UGr)
    go i (w:ws) t nodes edges
      = go (i + 1) ws (trieMapInsert w i t) (i : nodes)
      $ concat [ [(i, iN), (iN, i)]
               | iN <- findNeighbors w t ]
        ++ edges

findTransitions source target words
  = case (mbSrcI, mbTrgI)
      of (Just (Leaf srcI), Just (Leaf trgI)) -> nths (esp srcI trgI graph) words
         _                                    -> []
  where
    (trie, graph) = buildTrieMapAndGraph words
    
    mbSrcI = trieMapLookup source trie
    mbTrgI = trieMapLookup target trie

findLongestTransition words = map (flip nths words)
                            $ takeWhile ((== plen) . length) paths
  where
    (_, graph) = buildTrieMapAndGraph words
    paths = sortWith (negate . length)
          $ concatMap (flip bft graph)
          $ nodes graph
    plen  = length $ head paths

-- main task
main = do hSetEncoding stdin utf8
          (w1 : w2 : _) <- getArgs
          interact (unlines . findTransitions w1 w2 . lines)

-- "generality"-test task

{-
main = do hSetEncoding stdin utf8
          input <- hGetContents stdin
          mapM_ (\l -> mapM_ putStrLn l
                    >> putStrLn "")
            $ findLongestTransition
            $ lines input
-}