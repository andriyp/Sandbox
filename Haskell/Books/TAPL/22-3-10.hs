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

inf :: Term -> ([Constr], Type)
inf = fst . f [] [] [1..]
  where
    f ctx ctr tgs (LamT v ty t) = ((ctr', ty :-> ty'), tgs')
      where
        ((ctr', ty'), tgs') = f ((v, ty) : ctx) ctr tgs t
        
    f ctx ctr tgs (AppT l r) = ( ( (lty, rty :-> newTyVar) : (lctr ++ ctr ++ rctr)
                                 , newTyVar )
                               , tgs''')
      where
        ((lctr, lty), tgs')       = f ctx [] tgs  l
        ((rctr, rty), (x:tgs''')) = f ctx [] tgs' r
        
        newTyVar = TyVar ("X" ++ show x)
    
    f ctx ctr tgs (VarT v) = ((ctr, ty), tgs)
      where
        Just ty = lookup v ctx
    
    

testT = LamT "x" (TyVar "X")
      $ LamT "y" (TyVar "Y")
      $ LamT "z" (TyVar "Z")
      $ AppT (AppT (VarT "x") (VarT "z"))
             (AppT (VarT "y") (VarT "z"))

test = inf testT