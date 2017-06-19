module Lib where

import Prelude hiding ((<|>), (<*>), (<$>))
import Data.Char

infixl 3 <|>
infixl 4 <*>

type Parser s r = [s] -> [(r, [s])]

symbola :: Parser Char Char
symbola [] = []
symbola (x:xs) = if x == 'a' then [('a',xs)]
                 else []

symbol :: Eq a => a -> Parser a a
symbol s [] = []
symbol s (x:xs) = if x == s then [(s,xs)]
                  else []

satisfy :: (s -> Bool) -> Parser s s
satisfy p [] = []
satisfy p (x:xs) | p x = [(x,xs)]
                 | otherwise = []

token :: (Eq s) => [s] -> Parser s [s]
token t [] = []
token t inp | take (length t) inp == t = [(t, drop (length t) inp)]
            | otherwise = []

-- exToken = token "for" "for (i = 0 ..."

succeed :: r -> Parser s r
succeed r inp = [ ( r , inp) ]

(<|>) :: Parser s a -> Parser s a -> Parser s a
(p <|> q) inp = p inp ++ q inp

exOr = satisfy isDigit
   <|> symbol 'c'

(<*>) :: Parser s (a->b) -> Parser s a -> Parser s b
(p <*> q) inp = [ ((f v), ys)
                | (f,xs) <- p inp
                , (v,ys) <- q xs
                ]

(<$>) :: (a->r) -> Parser s a -> Parser s r
(f <$> p) inp = [ (f v, xs)
                | (v , xs) <- p inp
                ]

-- ((),"for")
auxEspacos x = x ==' ' || x =='\n' || x =='\t' || x=='\r' || x== '\0'

espacos x =  [((),dropWhile(auxEspacos) x)]

spaces = (\_ _ -> ()) <$> symbol ' ' <*> spaces
     <|> (\_ -> ()) <$> symbol ' '

spaces' = (\_ _ -> ()) <$> symbol ' ' <*> spaces'
       <|> succeed()

-- token' t = (\_ r _ -> r) <$> spaces' <*> token t <*> spaces'
token' t = (\_ r _ -> r) <$> espacos <*> token t <*> espacos

-- symbol' t = (\_ r _ -> r) <$> spaces' <*> symbol t <*> spaces'
symbol' t = (\_ r _ -> r) <$> espacos <*> symbol t <*> espacos

oneOrMore p = f <$> p <*> oneOrMore p
           <|> g <$> p
           where f r rs = r:rs
                 g r    = [r]

zeroOrMore p = (\r rs -> r:rs) <$> p <*> zeroOrMore p
          <|> succeed []

enclosedBy a p f = g <$> a <*> p <*> f
            where g x y z = y

delimitedPerParenthesis p = enclosedBy (symbol '(') p (symbol ')')

blocoC p = enclosedBy (symbol '{') p (symbol '}')
blocoPascal p = enclosedBy (token "BEGIN") p (token "END")

spacesD =  f <$> spacesD <*> symbol ' '
       <|> g <$> symbol ' '
           where f a b = a++[b]
                 g a   = [a]

pString :: Parser Char String
pString = oneOrMore $ satisfy (\x -> isAlpha x)

pInt = oneOrMore $ satisfy (\x -> isDigit x)

pBool = True

pTypeInt' =  token' "int"
pTypeChar' = token' "char"
pTypeBool' = token' "bool"
