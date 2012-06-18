-- Arrows and Computation

import Prelude hiding ( (.), id, zip )
import Data.Zip
import Data.Monoid
import Control.Monad
import Control.Arrow
import Control.Category
import Control.Category.Braided
import Control.Category.Associative
import Control.Applicative

list x = [x]

dup f x = f x x

assoc ((a,b),c) = (a,(b,c))

{- arrowLaws:
     1. arr id = id
     2. arr (f >>> g) = arr f >>> arr g
     3. first (arr f) = arr (first f)
     4. first (f >>> g) = first f >>> first g
     5. first f >>> arr fst = arr fst >>> f
     6. first f >>> arr (id *** g) = arr (id *** g) >>> first f
     7. first (first f) >>> arr assoc = arr assoc >>> first f
-}

law (l,r) run = run l &&& run r

law3 f   = law ( first (arr f), arr (first f) )         
law4 f g = law ( first (f >>> g), first f >>> first g )
law5 f   = law ( first f >>> arr fst, arr fst >>> f )

-- Ex #1
newtype Reader e i o = R ((e, i) -> o)

instance Category (Reader e) where
  id            = R snd
  (R f) . (R g) = R $ \(e, i) -> f (e, (g (e, i)))

instance Arrow (Reader e) where
  arr f       = R (f . snd)
  first (R f) = R $ \(e, (i1, i2)) -> (f (e, i1), i2)

newtype Writer s i o = W (i -> (s, o))

instance Monoid s => Category (Writer s) where
  id            = W ((,) mempty)
  (W f) . (W g) = W $ \i -> let (s,  o)  = g i
                                (s', o') = f o
                             in (s <> s', o')

instance Monoid s => Arrow (Writer s) where
  arr f       = W ((,) mempty . f)
  first (W f) = W $ \(i1,i2) -> let (s, o) = f i1
                                 in (s, (o, i2))

-- Ex #2
newtype ListMap i o = LM { runLM :: [i] -> [o] }

instance Category ListMap where
  id              = LM id
  (LM f) . (LM g) = LM (f . g)

instance Arrow ListMap where
  arr f        = LM (f <$>)
  first (LM f) = LM $ \xs -> zip (f $ fst <$> xs) (snd <$> xs)

chkLM aplaw = aplaw runLM

xs = [ (1,2), (3,4), (5,6) ]

lm_law3 = chkLM (law3 succ)           xs -- ok
lm_law4 = chkLM (dup law4 (arr succ)) xs -- ok

lmTl = LM tail

lm_law4' = chkLM (law4 lmTl lmTl) xs -- ok
lm_law5' = chkLM (law5 lmTl)      xs -- ok

lmSum = LM $ \x -> [sum x]

lm_law4'' = chkLM (law4 lmSum lmSum) xs

-- ... wtf? i dunno why LM is not valid arrow
-- todo: ask stackoverflow

-- Ex #3
data Stream a = Cons a (Stream a)

newtype StreamMap i o = SM (Stream i -> Stream o)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Zip Stream where
  zip (Cons x xs) (Cons y ys) = Cons (x, y) (zip xs ys)

instance Category StreamMap where
  id              = SM id
  (SM f) . (SM g) = SM (f . g)

instance Arrow StreamMap where
  arr f        = SM (fmap f)
  first (SM f) = SM $ \xs -> zip (f $ fst <$> xs) (snd <$> xs)

-- Ex #7
newtype NonDet i o = ND (i -> [o])  

instance Category NonDet where
  id              = ND list
  (ND f) . (ND g) = ND (f <=< g)

instance Arrow NonDet where
  arr f        = ND (list . f)
  first (ND f) = ND (uncurry zip . (f *** repeat))

instance ArrowChoice NonDet where
  left (ND f) = ND (either ((Left <$>) . f) (list . Right))

newtype State s i o = ST ((s, i) -> (s, o))

instance Category (State s) where
  id              = ST id
  (ST f) . (ST g) = ST (f . g)

instance Arrow (State s) where
  arr f        = ST (f <$>)
  first (ST f) = ST (associate . (f *** id) . disassociate)

instance ArrowChoice (State s) where
  left (ST f) = ST g
    where
      g (s, Left  x) = Left <$> f (s, x)
      g (s, Right x) = (s, Right x)

instance ArrowChoice StreamMap where
  left (SM f) = SM ((Left <$>) . f . g)
    where
      g (Cons (Left x) xs) = Cons x (g xs)
      g (Cons _        xs) = g xs

-- Ex #9 - this is my favorite atm - kinda tricky stuff ^.^
newtype Except a i o = E (a i (Either String o))

instance ArrowChoice a => Category (Except a) where
  id            = E (arr Right)
  (E f) . (E g) = E (g >>> right f >>^ either Left id)  
  -- (E f) . (E g) = E (arr (either Left id) . right f . g)

instance ArrowChoice a => Arrow (Except a) where
  arr f       = E (arr (Right . f))
  first (E a) = E (first a >>^ \(e,b) -> flip (,) b <$> e)  
  -- first (E a) = E (arr (\(e,b) -> (flip (,) b) <$> e) . first a)
  -- first (E a) = E (arr (uncurry fmap . (flip (,) *** id) . swap) . first a)

