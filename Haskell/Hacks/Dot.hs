module Hacks.Dot where

import qualified Prelude as P
import Prelude hiding ( (.) )

import Data.Char
import Data.List

o . f = f o

main = "Roses are red, violets are blue"
         . map toLower
         . nub
         . sort
         . putStrLn