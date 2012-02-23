module Sandbox.Data.Hash where
import Data.Array.ST

data ChainedHashTable s elem index chain = 
  CHT { table :: STArray s index (chain elem)
      , hfunc :: elem -> index
      }

class ChainedHash h where
  get :: ChainedHashTable -> key -> elem
  get cht key = 1
  put :: ChainedHashTable -> key -> elem -> ChainedHashTable

test = do let ht = newCHT