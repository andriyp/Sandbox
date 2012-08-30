module PHOAS where

-- open import Level
open import Function hiding (id)
open import Data.Unit
open import Data.Nat hiding (suc) renaming (ℕ to Nat)

module Untyped where
  open import Data.Bool

  data Term (V : Set) : Set where
    var : V → Term V
    abs : (V → Term V) → Term V
    app : Term V → Term V → Term V

  id : ∀ {V} → Term V
  id = abs (λ x → var x)
    
  freeVarCount : ((V : Set) → Term V) → Nat
  freeVarCount term = go (term ⊤)
    where
      go : Term ⊤ → Nat    
      go (var _)   = 1
      go (abs f)   = go (f tt)
      go (app x y) = go x + go y
  
  η-reducible? : ((V : Set) → Term V) → Bool
  η-reducible? term = top (term Bool)
    where
      TB→B = Term Bool → Bool
      top : TB→B; mid : TB→B; bot : TB→B
      
      top (abs f) = mid (f false)
      top _       = false
      
      mid (app x (var false)) = bot x
      mid _                   = false
      
      bot (var b)   = b
      bot (abs f)   = bot (f true)
      bot (app x y) = bot x ∧ bot y

  subst : ∀ {V} → (Term (Term V)) → Term V
  subst (var x)   = x
  subst (abs f)   = abs (λ x → subst (f (var x)))
  subst (app x y) = app (subst x) (subst y)

module SimplyTyped→CPS where

  data Type : Set where
    Bool : Type
    _⇒_ : Type → Type → Type

  data Term (V : Type → Set) : Type → Set where
    var : {T : Type} → (V T) → Term V T
    app : {A B : Type} → Term V (A ⇒ B) → Term V A → Term V B
    abs : {T : Type} → (V T → Term V T) → Term V T

    true  : Term V Bool
    false : Term V Bool

  data CPS-Type : Set where
    CPS-Bool : CPS-Type
    _→0      : CPS-Type → CPS-Type
    _×_      : CPS-Type → CPS-Type → CPS-Type

    -- data CPS-Term : Set where
    
  ⌊_⌋ : Type → CPS-Type
  ⌊  Bool  ⌋ = CPS-Bool
  ⌊ A ⇒ B ⌋ = (⌊ A ⌋ × (⌊ B ⌋ →0)) →0
  
  -- ⌞ var x ⌟ = halt x