{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
module Main where

import Text.Peggy hiding ( Expr, parse )
import Text.Peggy.LeftRec
import Prelude

type Id = String

data Prog = Prog Id [Decl] Stmt                                        deriving (Show)
data Lit  = IntL Integer | StrL String                                 deriving (Show)
data Decl = VarD Id Id                                                 deriving (Show)
data Stmt = AssignS Id Expr | IfS Expr Stmt (Maybe Stmt) | SubS [Stmt] deriving (Show)
data Expr = AppE Expr Op Expr | LitE Lit | VarE Id                     deriving (Show)
data Op   = Add | Sub | Mul | Div | Eqv                                deriving (Show)

genParser [] $ removeLeftRecursion [peggy|

regionComment :: () =
  '(*' (regionComment / !'*)' . {()})* '*)' {()}

nm :: Id = [a-z_] [a-zA-Z0-9_]* { $1 : $2 }
ty :: Id = nm

strLit :: Lit = ( '\"' (!'\"' .)* '\"'
                / '\'' (!'\'' .)* '\'') { StrL $1 }
intLit :: Lit = [0-9]+                  { IntL (read $1) }

stmt :: Stmt
  = "if" expr "then" stmt ("else" stmt)? { IfS $1 $2 $3 }
  / "begin" (stmt ";")* "end"            { SubS $1 }
  / nm ":=" expr                         { AssignS $1 $2 }

op :: Op = "+" { Add } / "-" { Sub } 
         / "*" { Mul } / "/" { Div } / "=" { Eqv }

expr :: Expr
  = expr op expr          { AppE $1 $2 $3 }
  / "(" expr ")"          { $1 }    
  / lit:(strLit / intLit) { LitE lit }
  / nm                    { VarE $1 }

program :: Prog
  = "program" nm ";"
      ("var" nm ("," nm)* ":" ty ";" { ($1 : $2, $3) })*
      stmt
    { Prog $1 
           (concatMap (\(vs,t) -> map (VarD t) vs) $2)
           $3 
    }
|]

parse code = parseString program "" code
check code = either (const True) (const False) code

main = print . parse =<< getContents

test = print $ parse
       "program yoba;                        \
       \var yobaint, lala : integer;         \
       \begin                                \
       \  if yobaint = 0 then begin          \
       \    c := \"ETO TI, YOBA?\"+10*20;    \
       \  end                                \
       \  else begin                         \
       \    c := 'ETO YA, YOBA!' / (30-20);  \
       \  end;                               \
       \  c := 60;                           \
       \end."
-- >
{-
Right (Prog "yoba" [VarD "integer" "yobaint",VarD "integer" "lala"]
        (SubS [IfS (AppE (VarE "yobaint") Eqv (LitE (IntL 0)))
                   (SubS [AssignS "c" (AppE (LitE (StrL "ETO TI, YOBA?")) Add
                           (AppE (LitE (IntL 10)) Mul (LitE (IntL 20))))]) 
                   (Just (SubS [AssignS "c" (AppE (LitE (StrL "ETO YA, YOBA!")) Div 
                                 (AppE (LitE (IntL 30)) Sub (LitE (IntL 20))))]))
              , AssignS "c" (LitE (IntL 60))
              ]))