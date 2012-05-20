{-# LANGUAGE TypeOperators, TupleSections #-}
import Prelude hiding (id, (.))
import Control.Applicative
import Control.Monad
import Control.Category
import Control.Arrow

newtype (:~>) i o = ArrOIO { runArrOIO :: i -> IO o }

instance Category (:~>) where
  id                  = ArrOIO return
  ArrOIO f . ArrOIO g = ArrOIO (f <=< g)

instance Arrow (:~>) where
  arr f            = ArrOIO (return . f)
  first (ArrOIO f) = ArrOIO (\(a,b) -> (,b) <$> f a)  
  
moo :: IO Integer :~> Integer
moo = ArrOIO (>>= \x -> print x >> return (x + 1))

foo :: IO Integer :~> Integer
foo = (moo >>^ return) >>> (moo >>^ return) >>> moo

susnoolee :: IO Integer -> IO Integer
susnoolee = runArrOIO foo