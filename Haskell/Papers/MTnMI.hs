-- Monad Transformers and Modular Interpreters

{-# LANGUAGE MultiParamTypeClasses, TypeOperators, TypeSynonymInstances,
             FlexibleInstances, FlexibleContexts, OverlappingInstances #-}

module Papers.MTnMI ( Subtype(..), (:+:) ) where

infixr 9 :+:

type (:+:) a b = Either a b

class Subtype a b where
  inj :: a -> b       -- injection
  prj :: b -> Maybe a -- projection

instance Subtype a (a :+: b) where
  inj = Left
  prj = either Just (const Nothing)

instance Subtype b (a :+: b) where
  inj = Right
  prj = either (const Nothing) Just

instance Subtype a b => Subtype a (c :+: b) where
  inj = Right . inj
  prj = either (const Nothing) prj

type A = Bool :+: String :+: Char

a1 :: A
a1 = inj True

a2 :: A
a2 = inj "something"

a3 :: A
a3 = inj '!'

type B = A :+: (A,A)

a4 :: B
a4 = inj (a1,a3)

type C = [[A] :+: B]

a5 :: C
a5 = [inj [a1], inj [a2], inj [a3], inj a4]