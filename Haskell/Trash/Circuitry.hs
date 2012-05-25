{-# LANGUAGE TupleSections, Arrows, NoMonomorphismRestriction #-}
import Prelude hiding (id, (.), or, and)
import Control.Category
import Control.Arrow

class Arrow a => ArrowState a where
  (-<-) :: o -> a i o -> a i o

class Arrow a => ArrowCircuit a where
  delay :: x -> a x x

data B  = B0 | B1 deriving Show
type B2 = (B, B)
type B3 = (B, B, B)

infixr 3 &.
infixr 2 |.

(&.) B0 _  = B0
(&.) B1 B1 = B1
(&.) _  _  = B0

(|.) B1 _  = B1
(|.) B0 B0 = B0
(|.) _  _  = B1

notB B0 = B1
notB B1 = B0

newtype MealyM i o
      = MealyM { runMealyM :: i -> (o, MealyM i o) }

execMealyM i m = snd (runMealyM m i)

instance Category MealyM where
  id                      = MealyM $ \i -> (i, id)
  (MealyM f) . (MealyM g) = MealyM $ \i ->
    let (gO, gSM) = g i
        (fO, fSM) = f gO
     in (fO, fSM . gSM)

instance Arrow MealyM where
  arr f            = MealyM $ \i -> (f i, arr f)
  first (MealyM f) = MealyM $ \(i1,i2) -> (,i2) *** first $ f i1

instance ArrowState MealyM where
  (-<-) x m = MealyM $ \_ -> (x, m)

instance ArrowCircuit MealyM where
  delay x = MealyM $ \i -> (x, delay i)

instance ArrowLoop MealyM where
  loop (MealyM f) = MealyM $ \i ->
    let (~(o, r), f') = f (i, r)
     in (o, loop f')

inv  = arr notB
or   = arr $ uncurry (|.)
nor  = inv <<< or
and  = arr $ uncurry (&.)
nand = inv <<< and

norInv = nor <<< (inv *** inv)

or3And = proc (b1, b2, b3, b4, b5, b6) -> do
  a1 <- and -< (b1, b2)
  a2 <- and -< (b3, b4)
  a3 <- and -< (b5, b6)
  o1 <- or  -< (a1, a2) 
  or -< (o1, a3)
  
out1 a = a >>^ fst

-- !!!

rsT = proc (s, r) -> do
  rec q   <- nor      -< (r |. inv s &. dq', q')
      q'  <- nor      -< (s |. inv r &. dq , q )
      dq  <- delay B0 -< q
      dq' <- delay B1 -< q'
  returnA -< (q, q')
    
{- todo: rewrite on garrows, simulate as interpretation of terms of sublanguage, and ideally it would be
rsT = proc (s, r) -> do
  rec q  <- nor -< (r, q')
      q' <- nor -< (s, q )
  returnA -< (q, q')
-}

rscT = proc (s, r, c) -> do
  cs <- norInv -< (s, c)
  cr <- norInv -< (r, c)
  rsT -< (cs, cr)

dT = proc (d, c) -> do
  d' <- inv -< d
  rscT -< (d, d', c)

invShiftReg = proc (a1, a2, a3, r, l, y, c) -> do
  let rr  =  r &. c &. inv (a1 |. a2 |. a3)
  rec d1  <- or3And   -< (r, rr &. dq2 |. q2, B0, l, a1, y)
      q1  <- out1 dT  -< (d1, c)
      dq1 <- delay B0 -< q1
      d2  <- or3And   -< (rr &. dq1 |. q1, l, r, rr &. dq3 |. q3, a2, y)
      q2  <- out1 dT  -< (d2, c)
      dq2 <- delay B0 -< q2
      d3  <- or3And   -< (r, B0, rr &. dq2 |. q2, l, a3, y)
      q3  <- out1 dT  -< (d3, c)
      dq3 <- delay B0 -< q3
  returnA -< q1

{- todo: rewrite on garrows, simulate as interpretation of terms of sublanguage, and ideally it would be
invShiftReg = proc (a1, a2, a3, r, l, y, c) -> do
  rec d1  <- or3And   -< (r, q2, B0, l, a1, y)
      q1  <- out1 dT  -< (d1, c)
      d2  <- or3And   -< (r, q3, q1, l, a2, y)
      q2  <- out1 dT  -< (d2, c)
      d3  <- or3And   -< (r, B0, q2, l, a3, y)
      q3  <- out1 dT  -< (d3, c)
  returnA -< q1
-}

loudlyEvalManyMealyM _ []      = return ()
loudlyEvalManyMealyM m (i:ins) = do
  let (o, m') = runMealyM m i
  print o
  loudlyEvalManyMealyM m' ins
  
main = do 
  putStrLn "# Simulating RS-trigger:"
  loudlyEvalManyMealyM rsT
    [ (B0, B0), (B1, B0), (B1, B0)
    , (B0, B1), (B1, B0), (B0, B0) ]     
  putStrLn "# Simulating D-trigger:"
  loudlyEvalManyMealyM dT
    [ (B1, B0), (B1, B1), (B0, B0) ]
  putStrLn "# Simulating inverse shift register:"
  loudlyEvalManyMealyM invShiftReg
    [ (B0, B1, B0, B0, B0, B1, B0)
    , (B1, B1, B0, B0, B0, B1, B1)
    , (B0, B0, B0, B1, B0, B0, B1)
    , (B0, B0, B0, B1, B0, B0, B1)
    , (B0, B0, B0, B1, B0, B0, B1)
    ]

{-
rsT :: MealyM B2 B
rsT = M (mkRS 0)
  where
    tran s   = (s, StateM (mkRS s))
    mkRS s i = case i of
      (0,0) -> tran s
      (0,1) -> tran (1,0)
      (1,0) -> tran (0,1)
      (1,1) -> error "RS-Trigger: Forbidden Input (1,1)!"

test = fst $ runStateM (snd $ runStateM rsT (1,0)) (0,1)
-}

{-

-}