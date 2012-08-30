module ATAPL where

open import Data.Bool
open import Data.Product
open import Data.Nat renaming (ℕ to Nat)

module Ex-2-1-1 (Matrix : Nat → Nat → Set)
                (Day-YM : Nat → Nat → Nat → Set)
                (Date   : Nat → Nat → Nat → Set)
  where
    mul  = {a b c : Nat} → Matrix a b → Matrix b c → Matrix a c
    date = (year : Nat) → (month : Nat) → (day : Nat)
         → month > 0 → month < 13 → Day-YM year month day
         → Date year month day

module Ex-2-1-2 {A B : Set} (P : A → B → Set) where
  axiom-of-choice = (a : A) → Σ B (P a) → Σ (A → B) (λ f → (x : A) → P x (f x))

module Ex-2-1-3 {A B C : Set} (_≡_ : C → C → Set) where
  pullback = (f : A → C) → (g : B → C) → Σ (A × B) (λ p → f (proj₁ p) ≡ g (proj₂ p))

module Ex-2-1-4 (Ty : Set) (_➾_ : Ty → Ty → Ty) where
  data Term : Ty → Set where    
    lam : {A B : Ty} → (Term A → Term B) → Term (A ➾ B)
    app : {A B : Ty} → Term (A ➾ B) → Term A → Term B