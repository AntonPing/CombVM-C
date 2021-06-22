-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.
module NoremParser (module NoremParser) where
import NoremRuntime
import Prelude hiding (error)
import Control.Applicative
import Data.Char


-- Basic definitions

data Result a =
     Ok a
   | Err String
   deriving (Show,Eq)

newtype Parser a = P { run :: String -> (String, Result a)}

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap f p = P $ \s ->
      case run p s of
         (rst, Ok v) -> (rst,Ok (f v))
         (rst, Err e) -> (rst,Err e)

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\s -> (s, Ok v))

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pf <*> px = P $ \s ->
      case run pf s of
         (rst, Ok vf) -> run (fmap vf px) rst
         (rst, Err e) -> (rst, Err e)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P $ \s ->
      case run p s of
         (rst, Ok v) -> run (f v) rst
         (rst, Err e) -> (rst, Err e)
   -- return :: a -> Parser a
   return v = P (\s -> (s, Ok v))

instance Alternative Parser where
   -- empty :: Parser a
   empty = P $ \s -> (s, Err "empty!")
   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P $ \s ->
      case run p s of
         (_, Err e) -> run q s
         (rst, Ok v) -> (rst, Ok v)

-- return have already defined
error :: String -> Parser a
error e = P $ \s -> (s, Err e)

parEOF :: Parser Char
parEOF = P $ \s -> case s of
   (x:xs) -> (s, Err "eof not match!") 
   []     -> ([], Ok '\n') 

try :: Parser a -> Parser a
try p = P $ \s ->
   case run p s of
      (rst, Ok v) -> (s, Ok v)
      (rst, Err e) -> (s, Err e)

satisfy :: Bool -> Parser ()
satisfy p = if p
   then P (\s -> (s,Ok () ))
   else P (\s -> (s,Err "not satisfy"))

readChar :: Parser Char
readChar = P $ \s -> case s of
   []     -> ([], Err "can't get char!") 
   (x:xs) -> (xs, Ok x)

satChar :: (Char -> Bool) -> Parser Char
satChar p = do
   c <- readChar
   satisfy (p c)
   return c

parChar :: Char -> Parser Char
parChar c = satChar (== c)

parString :: String -> Parser String
parString s =
   let fs = fmap parChar s
       mn = foldl1 (>>) fs in
   mn >> return s

parSpace :: Parser Char
parSpace = satChar isSpace

parDigit :: Parser Char
parDigit = satChar isDigit

parLower :: Parser Char
parLower = satChar isLower

parUpper :: Parser Char
parUpper = satChar isUpper

parLetter :: Parser Char
parLetter = satChar isAlpha

parAlphaNum :: Parser Char
parAlphaNum = satChar isAlphaNum

isLegal :: Char -> Bool
isLegal x = not $ x `elem`
   "\n\t\r (){}[],.;:\"\\"

parLegal :: Parser Char
parLegal = satChar isLegal

eatSpace :: Parser ()
eatSpace = do
   many parSpace
   return ()

parIdent :: Parser String
parIdent = do
   x  <- parLower
   xs <- many parAlphaNum
   return (x:xs)

parSymb :: Parser String
parSymb = do
   xs <- some parLegal
   return xs

parNat :: Parser Int
parNat = do
   xs <- some parDigit
   return (read xs)

parInt :: Parser Int
parInt = do
      parChar '-'
      n <- parNat
      return (-n)
   <|>
      parNat

word :: Parser a -> Parser a
word p = do
   eatSpace
   v <- p
   eatSpace
   return v

termFold :: Term -> Parser Term
termFold fun = do
      arg <- parTerm
      termFold (App fun arg)
   <|> do
      parChar ';'
      arg <- termList
      return (App fun arg)
   <|>
      return fun

termList :: Parser Term
termList = do
   app0 <- parTerm
   termFold app0

parTerm :: Parser Term
parTerm = do
      x <- word parSymb
      return (Var x)
   <|> do
      parChar '\\'
      x <- parSymb
      parChar '.'
      t <- parTerm
      return (Abs x t)
   <|> do
      parChar '('
      t <- termList
      parChar ')'
      return t

parMain :: Parser Term
parMain = do
      t <- termList
      eatSpace
      parEOF
      return t
   <|> do
      parChar '('
      t <- termList
      parChar ')'
      eatSpace
      parEOF
      return t


parse :: String -> Maybe Term
parse str =
   case run parMain str of
      (_, Ok t) -> Just t
      (_, Err e) -> Nothing
   