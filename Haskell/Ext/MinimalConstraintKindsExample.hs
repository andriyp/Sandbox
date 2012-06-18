{-# LANGUAGE ConstraintKinds, FlexibleInstances #-}
data C c = forall a. c a => C a
instance Show (C Show) where show (C a) = show a
data Y = Y { v1 :: C Show, v2 :: C Show }
instance Show Y where show (Y a b) = show a ++ " | " ++ show b
x = show $ v1 $ Y (C 1) (C 2)