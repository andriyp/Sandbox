{-# LANGUAGE TypeOperators #-}

type Id = String

data Term = VarT Id
          | AppT Term Term
          | LamT Id Type Term
          deriving (Show)

data Type = TyNat
          | TyBool
          | Type :-> Type
          | TyVar Id
          deriving (Show)

type Constr = (Type, Type)

-- extremely fast-n-dirty style

inf :: Term -> ([Constr], Type)
inf = fst . f [] [1..]
  where
    f ctx tgs (LamT v ty t) = ((ctr', ty :-> ty'), tgs')
      where
        ((ctr', ty'), tgs') = f ((v, ty) : ctx) tgs t
        
    f ctx tgs (AppT l r) = ( ( (lty, rty :-> newTyVar) : (lctr ++ rctr)
                                 , newTyVar )
                               , tgs''')
      where
        ((lctr, lty), tgs')       = f ctx tgs  l
        ((rctr, rty), (x:tgs''')) = f ctx tgs' r
        
        newTyVar = TyVar ("X" ++ show x)
    
    f ctx tgs (VarT v) = (([], ty), tgs)
      where
        Just ty = lookup v ctx        

testT = LamT "x" (TyVar "X")
      $ LamT "y" (TyVar "Y")
      $ LamT "z" (TyVar "Z")
      $ AppT (AppT (VarT "x") (VarT "z"))
             (AppT (VarT "y") (VarT "z"))

test = inf testT