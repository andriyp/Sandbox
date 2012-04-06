-- CMEXPR

{-# LANGUAGE CPP #-}
#define OPTS OPTIONS
-- {-# OPTS -O3 #-}

import Data.List
import Data.Char
import Data.Maybe
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

char c   = charSat (== c)
oneOf cs = charSat (`elem` cs)

lparen = charSat (== '(')
rparen = charSat (== ')')

-- part above is my mini-parsec for copypasting to ideone or spoj

type Op = Char

data T = Var Char
       | App T Op T deriving Show

opPrecEQ o1 o2 = (o1 == '+' || o1 == '-')
              == (o2 == '+' || o2 == '-')

opPrecGE o1 o2 = (o1 == '*' || o1 == '/')
              || (o2 == '+' || o2 == '-')

uncurry2n3of3 f a (b,c) = f a b c

foldApp t []        = t
foldApp t [(o',t')] = App t o' t'

foldApp t ((o',t') : ots@((o'',t'') : _))
  | opPrecGE o' o'' = foldApp (App t o' t') ots
  | otherwise       = foldApp (App t o' (foldl' (uncurry2n3of3 App) t' l)) r
  where
    (l,r) = span (opPrecEQ o'' . fst) ots

term = foldApp <$> pv <*> many rapp
  where
    var   = Var <$> charSat isLetter
    op    = oneOf "+-*/^"
    rapp  = (,) <$> op <*> pv
    pterm = lparen *> term <* rparen
    pv    = pterm <|> var

reparentize _ (Var c) = ([c], 'T')

reparentize isFoldingMinus (App e1 op e2)
  | op `elem` "+-" &&
    isFoldingMinus = ("(" ++ s1 ++ [op] ++ s2 ++ ")", op)
  | op `elem` "+-" = (       s1 ++ [op] ++ s2,        op)
  | otherwise = (s1' ++ [op] ++ s2', op)
  where
    (s1,op1) = reparentize False       e1
    (s2,op2) = reparentize (op == '-') e2
    s1' | op1 `elem` "+-" = "(" ++ s1 ++ ")"
        | otherwise       = s1
    s2' | op2 `elem` "+-" || (op == '/' && op2 `elem` "*/") = "(" ++ s2 ++ ")"
        | otherwise = s2

pretty = fst . reparentize False

main = do n <- readLn
          forM_ [1..n] $ \_ -> do
            ln <- getLine
            putStrLn $ pretty $ parse term ln

-- main = interact (unlines . map (pretty . parse term) . tail . lines)

{-

term = foldl' (uncurry2n3of3 App) <$> pv <*> many rapp
  where
    var   = Var <$> charSat isLetter
    op    = Op  <$> oneOf "+-*/^"
    pterm = lparen *> term <* rparen
    rapp  = (,) <$> op <*> pv
    pv    = pterm <|> var

{-
term = (uncurry App <$> lapp <*> term) <|> pterm <|> var
  where
    var   = Var <$> charSat isLetter
    op    = Op  <$> oneOf "+-*/^"
    pterm = lparen *> term <* rparen
    lapp  = (,) <$> (var <|> pterm) <*> op
-}

test = last
     $ map pretty [ parse term s 
                  | s <- take 1000000
                       $ cycle [ "(a+(b*c))"
                               , "((a+b)*c)"
                               , "(a*(b*c))"
                               , "(a*(b/c)*d)"
                               , "((a/(b/c))/d)"
                               , "((x))"
                               , "(a+b)-(c-d)-(e/f)"
                               , "(a+b)+(c-d)-(e+f)" ]
                  ]

test = map pretty [ parse term s 
                  | s <- [ "(a+(b*c))"
                         , "((a+b)*c)"
                         , "(a*(b*c))"
                         , "(a*(b/c)*d)"
                         , "((a/(b/c))/d)"
                         , "((x))"
                         , "(a+b)-(c-d)-(e/f)"
                         , "(a+b)+(c-d)-(e+f)" ]
                  ]

data ParserResult r = Empty
                    | Consumed r
                    | Done r
                    deriving (Eq, Show)

instance Functor ParserResult where
  fmap _ Empty        = Empty
  fmap f (Consumed r) = Consumed (f r)
  fmap f (Done r)     = Done (f r)

newtype Parser s r = Parser { runParser :: s -> (ParserResult r, s) }

instance Functor (Parser s) where
  fmap f pa = Parser $ \s ->
    let (pr, s') = runParser pa s
     in (fmap f pr, s')

instance Monad (Parser [a]) where
  return r = Parser $ \s -> case s of [] -> (Done r, [])
                                      _  -> (Consumed r, s)
  pa >>= f = Parser $ \s ->
    let (pr, s') = runParser pa s
     in case pr of
      Empty      -> (Empty, s)
      Consumed r -> runParser (f r) s'
      Done r     -> runParser (f r) s'

instance Applicative (Parser [a]) where
  pure  = return
  (<*>) = ap

instance Alternative (Parser [a]) where
  empty     = Parser $ \s -> (Empty, s)
  p1 <|> p2 = Parser $ \s ->
    let r@(pr, s') = runParser p1 s
     in case pr of
      Done _     -> r
      Consumed _ -> case runParser p2 s
                      of r'@(Done _, _)  -> r'
                         _               -> r
      Empty      -> runParser p2 s

-- parse p = fromJust . fst . runParser p

charSat p = Parser f
  where f []                 = (Empty,      [])
        f [x]    | p x       = (Done x,     [])
        f (x:xs) | p x       = (Consumed x, xs)
                 | otherwise = (Empty,      xs)

lparen = charSat (== '(')
rparen = charSat (== ')')

data Op = Op Char deriving Show
data T = Var Char
       | App T Op T
       deriving Show

term = (Var <$> charSat isLetter)       
   <|> (App <$> (lparen *> term) <*> op <*> (term <* rparen))
   <|> (App <$> term <*> op <*> term)
  where
    op = Op <$> charSat (`elem` "+-*/^")

-- pretty (Var c) = [c]
-- pretty (App )

test = runParser term "(a+b)+(c-d)-(e+f)"

-- main = interact (unlines . map (postfix . parse term) . tail . lines)
-}