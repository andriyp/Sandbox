{-# LANGUAGE NoMonomorphismRestriction, NoImplicitPrelude, Rank2Types, UnicodeSyntax, LambdaCase, RebindableSyntax, OverloadedStrings #-}
module DynamicTyping where

import qualified Prelude as P
import Prelude hiding ((+), (-), (*), error, fromInteger, fromRational) -- (Num, Show, Read, show, (.), (>>))
import Prelude.Unicode

fi = P.fromIntegral

-- our tiny dynamic type system
data T = IntT Integer
       | FloT Float
       | StrT String
       | FunT (T → T)

instance Show T where
  show = \case
    IntT x → show x
    FloT x → show x
    StrT x → x
    FunT x → show "#<function>"

error ∷ T → a
error s = P.error (show s)

arithT ∷ (∀ a. Num a ⇒ a → a → a) → T → T → T

arithT o (IntT x) (IntT y) = IntT (fi x `o` fi y)
arithT o (IntT x) (FloT y) = FloT (fi x `o` y   )
arithT o (FloT x) (IntT y) = FloT (x    `o` fi y)
arithT o (FloT x) (FloT y) = FloT (x    `o` y   )
arithT _ _        _        = error "Incompatible types!"

(+), (*), (-), (@@) ∷ T → T → T

(+) = arithT (P.+)
(*) = arithT (P.*)
(-) = arithT (P.-)

x @@ y = StrT (show x P.++ show y)

-- here we throw away haskell types,
--   downshifting values to embedded dynamic subworld of T
fromInteger  = IntT
fromRational = FloT ∘ P.fromRational
fromString   = StrT

x ∷ T
x = 32 + 1.213 @@ " i'm so dynamic " @@ 12.345

y ∷ T
y = 1 + "typechecks perfectly…"

say x = P.putStrLn (show x)

main = say x >> say y
-- > 33.213 i'm so dynamic 12.345
-- > Exception: Incompatible types!

-- we can even throw away haskell arrows,
--   downshifting binary functions on downshifted values to embedded dynamic subworld of T
downshift ∷ (T → T → T) → T
downshift f = FunT (\x → FunT (f x))

(+%) ∷ T
(+%) = downshift (+)