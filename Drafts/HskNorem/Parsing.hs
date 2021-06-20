-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char
import Compiler

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

eof :: Parser ()
eof = P (\inp -> case inp of
                     []     -> [((),[])]
                     (x:xs) -> [])

space :: Parser ()
space = P ( \inp -> case inp of
      ('\n':xs)   -> [((),xs)]
      ('\t':xs)   -> [((),xs)]
      ('\r':xs)   -> [((),xs)]
      (' ':xs)   -> [((),xs)]
      _     -> [])

try :: Parser a -> Parser a
try p = P ( \inp ->
   case parse p inp of
      [] -> []
      xs -> map (\(a,s)->(a,inp)) xs)

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

isLegal :: Char -> Bool
isLegal x = not $ x `elem`
   "\n\t\r (){}[],.;:\"\\"

legal :: Parser Char
legal = sat isLegal

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


eatSpace :: Parser ()
eatSpace = do
      many space
      return ()
   <|>
      return ()

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat


token :: Parser a -> Parser a
token p = do eatSpace
             v <- p
             peekSpace
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

match :: String -> Parser String
match xs = token (string xs)

symbol :: Parser String
symbol = do
   xs <- some legal
   return xs

peek :: Char -> Parser Char
peek x = try $ char x

peekSpace :: Parser Char
peekSpace = try $ sat isSpace

scomb :: Parser Term
scomb = do
   char 'S'
   return S

kcomb :: Parser Term
kcomb = do
   char 'K'
   return K

icomb :: Parser Term
icomb = do
   char 'I'
   return I

termFold :: Term -> Parser Term
termFold fun = do
      eatSpace
      arg <- term
      eatSpace
      termFold (App fun arg)
   <|> do
      eatSpace
      char ';'
      arg <- termList
      return (App fun arg)
   <|>
      return fun


termList :: Parser Term
termList = do
   app0 <- term
   result <- termFold app0
   return result


term :: Parser Term
term = do
      space
      term
   <|> do
      x <- symbol
      return (Var x)
   <|> do
      char '\\'
      x <- symbol
      char '.'
      t <- term
      return (Abs x t)
   <|> do
      char '('
      result <- termList
      char ')'
      return result
   <|> scomb <|> kcomb <|> icomb

parTerm :: Parser Term
parTerm = do
   app0 <- term
   result <- termFold app0
   return result


parseTerm :: String -> Maybe Term
parseTerm str =
   let result = parse parTerm str in
   case result of
      [] -> Nothing
      [(t,"")] -> Just t
      [(t,_)] -> Nothing 
      (x:xs) -> Nothing 
