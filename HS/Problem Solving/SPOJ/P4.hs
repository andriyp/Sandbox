import Data.Char
import Data.Maybe
import Data.Functor
import Control.Monad
import Control.Applicative

newtype Parser s r = Parser { runParser :: s -> (Maybe r, s) }

instance Functor (Parser s) where
  fmap f pa = Parser $ \s ->
    let (mbr, s') = runParser pa s
     in (fmap f mbr, s')

instance Monad (Parser s) where
  return r = Parser $ \s -> (Just r, s)
  pa >>= f = Parser $ \s ->
    let (mbr, s') = runParser pa s
     in case mbr of
      Just r  -> runParser (f r) s'
      Nothing -> (Nothing, s)

instance Applicative (Parser s) where
  pure  = return
  (<*>) = ap

instance Alternative (Parser s) where
  empty     = Parser $ \s -> (Nothing, s)
  p1 <|> p2 = Parser $ \s ->
    let (mbr, s') = runParser p1 s
     in case mbr of
      Just _  -> (mbr, s')
      Nothing -> runParser p2 s

parse p = fromJust . fst . runParser p

charSat p = Parser f
  where f []                 = (Nothing, [])
        f (x:xs) | p x       = (Just x,  xs)
                 | otherwise = (Nothing, xs)

lparen = charSat (== '(')
rparen = charSat (== ')')

data Op = Op Char
data T = Var Char
       | App T Op T

term = (Var <$> charSat isLetter)
   <|> (App <$> (lparen *> term)
            <*> (Op <$> charSat (`elem` "+-*/^"))
            <*> (term <* rparen))

postfix (Var c)          = [c]
postfix (App x (Op o) y) = postfix x ++ postfix y ++ [o]

main = interact (unlines . map (postfix . parse term) . tail . lines)


-- test = postfix $ parse term "((a+t)*((b+(a+c))^(c+d)))"

-- > "at+bac++cd+^*"

{-
type Parser s r = State s (Maybe r)
runParser :: Parser s r -> s -> (Maybe r, s)
runParser = runState

newtype Parser s r = Parser { runParser :: s -> (Maybe r, s) }

instance Monad (Parser s) where
  return r = Parser $ \s -> (Just r, s)
  pa >>= f = Parser $ \s ->
    let (mbr, s') = runParser pa s
     in case mbr of
      Just r  -> runParser (f r) s'
      Nothing -> (Nothing, s)
-}


{-
instance Functor (Parser s) where
  fmap f (Parser s r) = Parser s (fmap f r)

instance Monoid s => Applicative (Parser s) where
  pure = Parser mempty . Just
  (Parser s1 r1) <*> (Parser s2 r2) = Parser (s1 `mappend` s2) (r1 <*> r2)

{-
(<|>) f g xs | p == []   = g xs
             | otherwise = (p,r)
  where (p,r) = f xs

(<+>) f g xs | p1 /= [] && p2 /= [] = (p1 p2,r2)
             | otherwise            = ([]   ,xs)
  where (p1,r1) = f xs
        (p2,r2) = g r1

-}

charSat :: (Char -> Bool)
charSat p = pure f
  where f []                 = ([]   ,[])
        f (x:xs) | p x       = ([[x]],xs)
                 | otherwise = ([]   ,xs)



(.$) f t xs = (t p,r)
  where (p,r) = f xs

expr = (lparen <+> (c1 <|> c2) <+> rparen)
  where
    c1 = (letter <+> op <+> letter) .$ (\[x,o,y] -> [x,y,o])
    c2 
-}
    
-- num =
-- parse x:xs