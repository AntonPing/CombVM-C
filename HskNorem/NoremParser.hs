-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.
module NoremParser (module NoremParser) where

import Prelude hiding (error)
import Control.Applicative
import Data.Char
import Compiler

-- Basic definitions

data Result a =
     Ok a
   | Err String

newtype Parser a = P { parse :: String -> (String, Result a)}

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap f p = P $ \s ->
      case parse p s of
         (rst, Ok v) -> (rst,Ok (f v))
         (rst, Err e) -> (rst,Err e)

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\s -> (s, Ok v))

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pf <*> px = P $ \s ->
      case parse pf s of
         (rst, Ok vf) -> parse (fmap vf px) rst
         (rst, Err e) -> (rst, Err e)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P $ \s ->
      case parse p s of
         (rst, Ok v) -> parse (f v) rst
         (rst, Err e) -> (rst, Err e)

-- Making choices
instance Alternative Parser where
   -- empty :: Parser a
   empty = P $ \s -> (s, Err "empty!")
   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P $ \s ->
      case parse p s of
         (_, Err e) -> parse q s
         (rst, Ok v) -> (rst, Ok v)

error :: String -> Parser a
error e = P $ \s -> (s, Err e)

anyChar :: Parser Char
anyChar = P $ \s -> case s of
   []     -> ([], Err "can't get char!") 
   (x:xs) -> (xs, Ok x)

eof :: Parser ()
eof = P $ \s -> case s of
   (x:xs) -> (s, Err "eof not match!") 
   []     -> ([], Ok ()) 

space :: Parser () 
space = P $ \s -> case s of
   ('\n':xs) -> (xs, Ok ())
   ('\t':xs) -> (xs, Ok ())
   ('\r':xs) -> (xs, Ok ())
   (' ':xs) ->  (xs, Ok ())
   _     -> (s, Err "space not match!")

try :: Parser a -> Parser a
try p = P $ \s ->
   case parse p s of
      (rst, Ok v) -> (s, Ok v)
      (rst, Err e) -> (s, Err e)

sat :: (Char -> Bool) -> Parser Char
sat p = do
   x <- anyChar
   if p x then return x
   else error "not sat"

char :: Char -> Parser Char
char c = sat (== c)

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

isLegal :: Char -> Bool
isLegal x = not $ x `elem`
   "\n\t\r (){}[],.;:\"\\"

legal :: Parser Char
legal = sat isLegal

string :: String -> Parser String
string []     = return []
string (x:xs) = do
   char x
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
   eof
   return result

parseTerm :: String -> Maybe Term
parseTerm str =
   let result = parse parTerm str in
   case result of
      (_, Ok t) -> Just t
      (_, Err e) -> Nothing