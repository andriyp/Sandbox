{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UnicodeSyntax, KindSignatures, LambdaCase #-}

import Prelude.Unicode

-- Ребята, нам нужно чтобы делался хлеб
{-
data Bread = Bread

bakeBread = Bread
-}

-- Нам нужно, чтобы хлеб не просто делался, а выпекался в печке
{-
data Oven (m ∷ * → *) = Oven

bakeBread ∷ Monad m ⇒ Oven m → m Bread
bakeBread oven = return Bread
-}

-- Нам нужно, чтобы печки были разных видов
{-
class Oven (o ∷ (* → *) → *) where
-}

-- Нам нужно, чтобы газовая печь не могла печь без газа
data GasOven m = GasOven { gasAvailable ∷ m Bool }

class Oven (o ∷ (* → *) → *) where
  canBake ∷ o m → m Bool

instance Oven GasOven where
  canBake = gasAvailable

-- Нам нужно, чтобы печки могли выпекать ещё и пирожки (отдельно - с мясом, отдельно - с капустой), и торты
data Bread   = Bread
data Cake    = Cake
data Patty f = Patty { filling ∷ f }

bake ∷ (Monad m, Oven oven) ⇒ oven m → (oven m → m bakery) → m bakery
bake oven wf = canBake oven >>= \case
  True  → wf oven
  False → error "Your oven is dead!"

bakeBread        oven = bake oven $ return ∘ const Bread
bakeCake         oven = bake oven $ return ∘ const Cake
bakeMeatPatty    oven = bake oven $ return ∘ const (Patty "Meat")
bakeCabbagePatty oven = bake oven $ return ∘ const (Patty "Cabbage")

-- Нам нужно, чтобы хлеб, пирожки, и торты выпекались по разным рецептам
class Recipe bakery ingredients where
  cook ∷ (Monad m, Oven oven) ⇒ oven m → ingredients → m bakery

data Meat    = Meat
data Cabbage = Cabbage
   
instance Recipe (Patty Meat) Meat where
  cook oven Meat = return (Patty Meat)

instance Recipe (Patty Cabbage) Cabbage where
  cook oven Cabbage = return (Patty Cabbage)

-- Нам нужно, чтобы в печи можно было обжигать кирпичи
data Brick = Brick

bakeBrick oven = bake oven $ return ∘ const Brick