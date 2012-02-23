{-# LANGUAGE ViewPatterns #-}
module Sandbox.Ext.ViewPatterns where

import Data.Sequence ((<|), (|>), viewl, viewr, ViewR(..), ViewL(..))
import qualified Data.Sequence as S

{-
   [ Abstracting from ADT's to their "views" ]
-}

-- standard left-view for Data.Sequence
lincr (viewl -> x :< xs) = (x + 1) <| xs
lincr _                  = S.empty

testLincr = lincr (S.fromList [1..5]) -- > fromList [2,2,3,4,5]

-- common pattern
data T = A1 | A2 | A3 | B1 | B2
       deriving (Eq, Show)

data TView = A | B 
           deriving (Eq)

letter :: T -> TView
letter t | t `elem` [A1,A2,A3] = A
         | otherwise           = B

mapA :: (T -> T) -> [T] -> [T]
mapA f (x@(letter -> l) : (mapA f -> xs')) -- right part is so-called "iterator-style" ^.^
  | l == A    = f x : xs'
  | otherwise =   x : xs'
mapA f [] = []

testMapA = mapA (const B2) [A1, A2, A3, B1, B2] -- > [B2,B2,B2,B1,B2]