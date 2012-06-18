module Numeric.Percent ( Percent ) where

(.:) f g x y = f (g x y)

newtype Percent = Percent Int deriving (Eq, Show, Read)

validate p@(Percent x)
  | (0 <= x) && (x <= 100) = p
  | otherwise              = error "Percent is int in range 0..100!"

under  f (Percent x) = Percent (f x)
under2 f (Percent x) = under   (f x)

instance Num Percent where

  fromInteger = validate . Percent . fromInteger

  (+)    = validate .: under2 (+)
  (*)    = validate .: under2 (*)
  abs    = id
  signum = const 1

-- primer4iki
quux :: Percent
quux = 98 + 1 -- > 99

-- foo :: Percent
foo = quux + 2 -- > Percent *** Exception: Percent is int in range 0..100!