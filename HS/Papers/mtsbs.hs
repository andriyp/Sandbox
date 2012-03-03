{-# LANGUAGE DeriveDataTypeable #-}
-- Monad Transformers Step by Step
module MTSBS where

import Prelude hiding ( lookup )

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Typeable
import Data.Maybe
import Data.Map

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return  = Identity
  m >>= f = f (runIdentity m)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

type Id = String

data Expr = LitE Integer
          | SumE Expr Expr
          | VarE Id
          | LamE Id Expr
          | AppE Expr Expr
          deriving (Show, Typeable)

type Env = Map Id Val

data Val = IntV Integer
         | CloV Env Id Expr
         deriving (Show)

-- test expression
testE = LitE 12 `SumE`
          (AppE (LamE "x" (VarE "x"))
                (LitE 4 `SumE` LitE 2))

-- reference implementation
eval0 :: Env -> Expr -> Val
eval0 e (LitE n)   = IntV n
eval0 e (SumE x y) = case (eval0 e x, eval0 e y) of
  (IntV a, IntV b) -> IntV (a + b)
  otherwise        -> error "Unsummable operands!"
eval0 e (VarE v)   = fromJust (lookup v e)
eval0 e (LamE v b) = CloV e v b
eval0 e (AppE x y) = let CloV e' v b = eval0 e x
                      in eval0 (insert v (eval0 e y) e') b

t0 = eval0 empty testE

-- moving to identity monad
type Eval1 = Identity

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Env -> Expr -> Eval1 Val
eval1 e (LitE n)   = return (IntV n)
eval1 e (SumE x y) = do IntV a <- eval1 e x
                        IntV b <- eval1 e y
                        return (IntV (a + b))
eval1 e (VarE v)   = return $ fromJust (lookup v e)
eval1 e (LamE v b) = return (CloV e v b)
eval1 e (AppE x y) = do CloV e' v b <- eval1 e x
                        yv <- eval1 e y
                        eval1 (insert v yv e') b

t1 = runEval1 (eval1 empty testE)
  
-- finally, moving to transformer - ErrorT
type Eval2 = ErrorT String Eval1

runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runErrorT

-- just copying eval1 and renaming calls
eval2a :: Env -> Expr -> Eval2 Val
eval2a e (LitE n)   = return (IntV n)
eval2a e (SumE x y) = do IntV a <- eval2a e x
                         IntV b <- eval2a e y
                         return (IntV (a + b))
eval2a e (VarE v)   = return $ fromJust (lookup v e)
eval2a e (LamE v b) = return (CloV e v b)
eval2a e (AppE x y) = do CloV e' v b <- eval2a e x
                         yv <- eval2a e y
                         eval2a (insert v yv e') b

-- adding robust error handling
eval2b :: Env -> Expr -> Eval2 Val

eval2b e (LitE n) = return (IntV n)

eval2b e (SumE x y) = do xv <- eval2b e x
                         yv <- eval2b e y
                         case (xv, yv) of
                           (IntV a, IntV b) -> return (IntV (a + b))
                           otherwise        -> throwError "Unsummable operands!"
                           
eval2b e (VarE v) = case lookup v e 
                      of Nothing -> throwError ("Unbound variable: " ++ v)
                         Just x  -> return x

eval2b e (LamE v b) = return (CloV e v b)

eval2b e (AppE x y) = do xv <- eval2b e x
                         yv <- eval2b e y
                         case xv of 
                           CloV e' v b -> eval2b (insert v yv e') b
                           otherwise   -> throwError "Non applicable value!"

t2 = runEval2 (eval2b empty testE)

-- hiding environment in reader monad
type Eval3 = ReaderT Env Eval2

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 e x = runIdentity $ runErrorT $ runReaderT x e

eval3 :: Expr -> Eval3 Val

eval3 (LitE n) = return (IntV n)

eval3 (SumE x y) = do xv <- eval3 x
                      yv <- eval3 y
                      case (xv, yv) of
                        (IntV a, IntV b) -> return (IntV (a + b))
                        otherwise        -> throwError "Unsummable operands!"

eval3 (VarE v) = do e <- ask
                    case lookup v e
                      of Nothing -> throwError ("Unbound variable: " ++ v)
                         Just x  -> return x

eval3 (LamE v b) = do e <- ask
                      return (CloV e v b)

eval3 (AppE x y) = do xv <- eval3 x
                      yv <- eval3 y
                      case xv of
                        CloV e v b -> local (const $ insert v yv e) (eval3 b)
                        otherwise  -> throwError "Non applicable value!"

t3 = runEval3 empty (eval3 testE)

-- adding state for tracking number of calls to eval
type Eval4 = ReaderT Env (ErrorT String (StateT Integer Identity))

tick :: (Num s, MonadState s m) => m ()
tick = get >>= put . (+ 1)

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 e i x = runIdentity $ flip runStateT i $ runErrorT $ runReaderT x e

eval4 :: Expr -> Eval4 Val

eval4 (LitE n) = tick >> return (IntV n)

eval4 (SumE x y) = do tick
                      xv <- eval4 x
                      yv <- eval4 y
                      case (xv, yv) of
                        (IntV a, IntV b) -> return (IntV (a + b))
                        otherwise        -> throwError "Unsummable operands!"

eval4 (VarE v) = do tick
                    e <- ask
                    case lookup v e
                      of Nothing -> throwError ("Unbound variable: " ++ v)
                         Just x  -> return x

eval4 (LamE v b) = do tick
                      e <- ask
                      return (CloV e v b)

eval4 (AppE x y) = do tick
                      xv <- eval4 x
                      yv <- eval4 y
                      case xv of
                        CloV e v b -> local (const $ insert v yv e) (eval4 b)
                        otherwise  -> throwError "Non applicable value!"

t4 = runEval4 empty 0 (eval4 testE)
-- > (Right (IntV 18),8)

-- Swap ErrorT and StateT
type Eval4' = ReaderT Env (StateT Integer (ErrorT String Identity))

runEval4' :: Env -> Integer -> Eval4' a -> Either String (a, Integer)
runEval4' e i x = runIdentity $ runErrorT $ flip runStateT i $ runReaderT x e

eval4' :: Expr -> Eval4' Val
eval4' = error "How to write eval4' without fully copying eval4?"

t4' = runEval4' empty 0 (eval4' testE)

-- Add logging
type Eval5 = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Identity)))

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 e i x = runIdentity $ flip runStateT i $ runWriterT $ runErrorT $ runReaderT x e

eval5 :: Expr -> Eval5 Val

eval5 (LitE n) = do tick
                    return (IntV n)

eval5 (SumE x y) = do tick
                      xv <- eval5 x
                      yv <- eval5 y
                      case (xv, yv) of
                        (IntV a, IntV b) -> return (IntV (a + b))
                        otherwise        -> throwError "Unsummable operands!"

eval5 (VarE v) = do tick
                    tell [v]
                    e <- ask
                    case lookup v e
                      of Nothing -> throwError ("Unbound variable: " ++ v)
                         Just x  -> return x

eval5 (LamE v b) = do tick
                      e <- ask
                      return (CloV e v b)

eval5 (AppE x y) = do tick
                      xv <- eval5 x
                      yv <- eval5 y
                      case xv of
                        CloV e v b -> local (const $ insert v yv e) (eval5 b)
                        otherwise  -> throwError "Non applicable value!"

t5 = runEval5 empty 0 (eval5 testE)

-- adding IO
type Eval6 = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO)))

runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 e i x = flip runStateT i $ runWriterT $ runErrorT $ runReaderT x e

eval6 :: Expr -> Eval6 Val

eval6 x = do tick
             liftIO $ putStrLn $ "Evaluating " 
               ++ tyConName (typeRepTyCon (typeOf x))
               ++ "..."
             eval6' x

eval6' (LitE n) = return (IntV n)

eval6' (SumE x y) = do xv <- eval6 x
                       yv <- eval6 y
                       case (xv, yv) of
                         (IntV a, IntV b) -> return (IntV (a + b))
                         otherwise        -> throwError "Unsummable operands!"

eval6' (VarE v) = do tell [v]
                     e <- ask
                     case lookup v e
                       of Nothing -> throwError ("Unbound variable: " ++ v)
                          Just x  -> return x

eval6' (LamE v b) = do e <- ask
                       return (CloV e v b)

eval6' (AppE x y) = do xv <- eval6 x
                       yv <- eval6 y
                       case xv of
                         CloV e v b -> local (const $ insert v yv e) (eval6 b)
                         otherwise  -> throwError "Non applicable value!"

t6 = runEval6 empty 0 (eval6 testE)