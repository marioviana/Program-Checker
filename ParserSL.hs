module ParserSL where

import Lib
import Prelude hiding ((<|>), (<*>), (<$>))
import Data.Char

{- Grupo 1
   Gramática SL -}

data SL = Program String [Pre] [Decl] [Inst] [Post]
        deriving Show

data Pre = Pre Boolean
         deriving (Show, Eq, Ord)

data Post = Post Boolean
         deriving (Show, Eq, Ord)

data Decl = Atr  TypeDecl String Expr
          | AtrT TypeDecl String
          | AtrS TypeDecl [Expr]
          deriving (Show , Eq , Ord)

data TypeDecl = Int
              | Char
              | Bool
              deriving (Show , Eq , Ord)

data Inst = Assign      String Expr
          | IfThenElse  Boolean [Inst] [Inst]
          | For         Decl Boolean Expr [Inst]
          | While       Boolean [Inst]
          | Read        Expr
          | Print       Expr
          | Return      Expr
          deriving (Show , Eq , Ord)

data Expr = Const Integer
          | Var   String
          | Add   Expr Expr
          | Mul   Expr Expr
          | Div   Expr Expr
          | Sub   Expr Expr
          | Same  Expr Expr
          deriving (Show , Eq , Ord)

data Boolean = Greater    Expr Boolean
             | Less       Expr Boolean
             | And        Expr Boolean
             | Orl        Expr Boolean
             | Equal      Expr Boolean
             | Different  Expr Boolean
             | Expr       Expr
             deriving (Show , Eq , Ord)

{- Grupo 2
   Parser SL -}

parser = f <$> token' "pre" <*> pres <*> token' "program" <*> pString <*> symbol' '{' <*> decls <*> spaces' <*> insts <*> symbol' '}' <*> token' "post" <*> posts
     where f _ a _ b _ c _ d _ _ e = Program b a c d e

pres = zeroOrMore pre

pre = f <$> boolean <*> symbol' ';'
    where f a _ = Pre a

posts = zeroOrMore post

post = f <$> boolean <*> symbol' ';'
        where f a _ = Post a

decls = oneOrMore decl

varios = oneOrMore vario

vario = f <$> symbol' ',' <*> expr
   where f _ s = s

decl =  f  <$> pTypeInt'  <*> pString <*> symbol' '=' <*> expr <*> symbol' ';'
    <|> g  <$> pTypeChar' <*> pString <*> symbol' '=' <*> expr <*> symbol' ';'
    <|> h  <$> pTypeBool' <*> pString <*> symbol' '=' <*> expr <*> symbol' ';'
    <|> i  <$> pTypeInt'  <*> pString <*> symbol' ';'
    <|> j  <$> pTypeChar' <*> pString <*> symbol' ';'
    <|> k  <$> pTypeBool' <*> pString <*> symbol' ';'
    <|> p1 <$> pTypeInt'  <*> expr <*> varios <*> symbol' ';'
    <|> p2 <$> pTypeChar' <*> expr <*> varios <*> symbol' ';'
    <|> p3 <$> pTypeBool' <*> expr <*> varios <*> symbol' ';'
     where f _ b _ c _ =  Atr Int b c
           g _ b _ c _ =  Atr Char b c
           h _ b _ c _ =  Atr Bool b c
           i _ b _     =  AtrT Int b
           j _ b _     =  AtrT Char b
           k _ b _     =  AtrT Bool b
           p1 _ b c _  =  AtrS Int ([b]++c)
           p2 _ b c _  =  AtrS Char ([b]++c)
           p3 _ b c _  =  AtrS Bool ([b]++c)

insts = oneOrMore inst

inst =  f  <$> token' "Print"  <*> expr <*> symbol' ';'
     <|> g <$> pString <*> symbol' '=' <*> expr <*> symbol' ';'
     <|> h <$> token' "if" <*> symbol' '(' <*> boolean <*> symbol' ')' <*> token' "then" <*> symbol' '{'
         <*> insts <*> symbol' '}' <*> token' "else" <*> symbol' '{' <*> insts <*> symbol' '}'
     <|> i <$> token' "if" <*> symbol' '(' <*> boolean <*> symbol' ')' <*> token' "then" <*> symbol' '{'
         <*> insts <*> symbol' '}'
     <|> j <$> token' "return" <*> expr <*> symbol' ';'
     <|> k <$> token' "read" <*> expr <*> symbol' ';'
     <|> l <$> token' "for" <*> symbol' '(' <*> decl <*> boolean <*> symbol' ';'
         <*> expr <*> symbol' ')' <*> symbol' '{' <*> insts <*> symbol' '}'
     <|> m <$> token' "while" <*> symbol' '(' <*> boolean <*> symbol' ')' <*> symbol' '{' <*> insts <*> symbol' '}'
   where f _ b _  =                   Print b
         g a b c _  =                 Assign a c
         h _ _ a _ _ _ b _ _ _ c _  = IfThenElse a b c
         i _ _ a _ _ _ b _ =          IfThenElse a b []
         j _ a _ =                    Return a
         k _ a _ =                    Read a
         l _ _ a b _ c _ _ d _ =      For a b c d
         m _ _ a _ _ b _ =            While a b

expr = id <$> expressao
    <|> f <$> expressao <*> symbol' '*' <*> expr
    <|> g <$> expressao <*> symbol' '+' <*> expr
    <|> h <$> expressao <*> symbol' '/' <*> expr
    <|> i <$> expressao <*> symbol' '-' <*> expr
    <|> j <$> expressao <*> symbol' '=' <*> expr
  where f l _ r = Mul l r
        g l _ r = Add l r
        h l _ r = Div l r
        i l _ r = Sub l r
        j l _ r = Same l r

expressao =  f <$> pString
         <|> g <$> pInt
  where f a = Var a
        g a = Const (read a :: Integer)

boolean = (\a -> Expr a) <$> expr
        <|> (\a _ b -> Less a b)      <$> expr <*> symbol' '<' <*> boolean
        <|> (\a _ b -> Greater a b)   <$> expr <*> symbol' '>' <*> boolean
        <|> (\a _ b -> Equal a b)     <$> expr <*> token' "==" <*> boolean
        <|> (\a _ b -> Different a b) <$> expr <*> token' "!=" <*> boolean
        <|> (\a _ b -> And a b)       <$> expr <*> token' "&&" <*> boolean
        <|> (\a _ b -> Orl a b)       <$> expr <*> token' "||" <*> boolean


{- Grupo 3
   Programas Sl -}

sl1 = x
   where ((x,y):xs) = parser "pre a>c; b>c; program a { int aux; int b; Print aux; Print r; } post a==5;"

sl2 = x
   where ((x,y):xs) = parser "program a { int aux;int b; if(a>e) then {Print aux;} else{r=r;} }"

sl3 = x
   where ((x,y):xs) = parser "program a {int aux=4;int b=t;aux= 10;Print aux; Print 10+5;if(x>5) then {b=5;} else{b=6;} }"

sl4 = x
   where ((x,y):xs) = parser "program a {int aux=4;int b=t;aux= 10;Print aux; while(a>3){aux=10;Print t;}Print t; }"

sl5 = x
   where ((x,y):xs) = parser "program a {int aux=4;int b=t;aux= 10;Print aux; for(int a=1;a>3;a=4){aux=10;Print t;}Print t; }"

sl6 = x
   where ((x,y):xs) = parser "program a {int aux=4;int b=t;aux= 10;Print aux; for(int a=1;a>3;a=4){while(a>4){r=t;}aux=10;Print t;}Print t; }"
