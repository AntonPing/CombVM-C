
module NoremParser (module NoremParser) where
import Prelude hiding (error)
import NoremRuntime
import Control.Applicative
import Data.Maybe
import Data.Char
import Debug.Trace

-- Basic definitions

data Result a =
     Ok a
   | Err String
   deriving (Show,Eq)

newtype Parser a = P { runP :: String -> (String, Result a)}

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap f p = P $ \s ->
      case runP p s of
         (rst, Ok v) -> (rst,Ok (f v))
         (rst, Err e) -> (rst,Err e)

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\s -> (s, Ok v))

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pf <*> px = P $ \s ->
      case runP pf s of
         (rst, Ok vf) -> runP (fmap vf px) rst
         (rst, Err e) -> (rst, Err e)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P $ \s ->
      case runP p s of
         (rst, Ok v) -> runP (f v) rst
         (rst, Err e) -> (rst, Err e)
   -- return :: a -> Parser a
   return v = P (\s -> (s, Ok v))

instance Alternative Parser where
   -- empty :: Parser a
   empty = P $ \s -> (s, Err "empty!")
   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P $ \s ->
      case runP p s of
         (_, Err e) -> runP q s
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
   case runP p s of
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

parAlpha :: Parser Char
parAlpha = satChar isAlpha

parAlphaNum :: Parser Char
parAlphaNum = satChar isAlphaNum

isLegal :: Char -> Bool
isLegal x = not $ x `elem`
   "\n\t\r (){}[],.;:\"\\"

parLegal :: Parser Char
parLegal = satChar isLegal

parDelim :: Parser Char
parDelim = satChar (not . isLegal) <|> parEOF

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
termFold fun = eatSpace >>
   do
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

opMap :: [(String,Term)]
opMap = 
   [("!", Uniop FNot),
   ("~", Uniop FNeg),
   ("if", Uniop FIf),
   ("+", Binop FAdd),
   ("-", Binop FSub),
   ("*", Binop FMul),
   ("/", Binop FDiv),
   ("==", Binop FEql),
   (">", Binop FGtr),
   ("<", Binop FLss),
   (">=", fromJust $ parse "\\x.\\y. ! (< x y)"),
   ("<=", fromJust $ parse "\\x.\\y. ! (> x y)"),
   ("true", Data (DBool True)),
   ("false", Data (DBool False))]

parTerm :: Parser Term
parTerm = eatSpace >>
   do
      n <- parInt
      try parDelim
      return (Data (DInt n))
   <|> do
      parChar '\\'
      x <- parSymb
      parChar '.'
      t <- termList
      return (Abs x t)
   <|> do
      str <- parString "let"
      some parSpace
      x <- parSymb
      some parSpace
      t1 <- parTerm
      t2 <- parTerm
      return (App (Abs x t2) t1)
   <|> do 
      str <- parString "letrec"
      some parSpace
      x <- parSymb
      some parSpace
      t1 <- parTerm
      t2 <- parTerm
      return (App (Abs x t2) (App Y (Abs x t1)))
   <|> do
      x <- parSymb
      try parDelim
      case lookup x opMap of
         Just op -> return op
         Nothing -> return (Var x)
   <|> do
      parChar '('
      t <- termList
      parChar ')'
      return t

parMain :: Parser Term
parMain = eatSpace >>
   do
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
   case runP parMain str of
      (_, Ok t) -> Just t
      (_, Err e) -> Nothing
   