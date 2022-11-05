module Frontend.Parser where

import Frontend.Token
import Utils.Symbol
import Utils.Syntax


import Text.Parsec (parse, ParseError)
import Debug.Trace (trace)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Utils.TopLevel (TopLevel, MonadTopLevel, InternStr)
import Control.Monad.RWS (MonadWriter)
import Control.Monad.Writer
import Control.Applicative

data ParError = ParError
    deriving (Eq,Show)

data ParState = ParState
    { stream :: [(Token,Span)]
    , consumed :: Bool
    } deriving (Eq,Show)

data ErrorMsg = ErrorMsg String
    deriving (Eq,Show)

newtype Parser a = Parser
    { unParser :: WriterT [ErrorMsg] (ExceptT ParError (StateT ParState TopLevel)) a }
    deriving
        ( Functor, Applicative, Monad
        , MonadWriter [ErrorMsg]
        , MonadError ParError
        , MonadState ParState
        , MonadTopLevel
        , MonadIO
        )

runParser :: Parser a -> ParState -> TopLevel (Either ParError (a, [ErrorMsg]), ParState)
runParser (Parser m) = (runStateT . runExceptT . runWriterT) m

evalParser :: Parser a -> ParState -> TopLevel (Either ParError (a, [ErrorMsg]))
evalParser (Parser m) = (evalStateT . runExceptT . runWriterT) m

mkParState :: [(Token,Span)] -> ParState
mkParState str = ParState
    { stream = str
    , consumed = False
    }

-- runParserStr :: Parser a -> [(Token, Span)] -> TopLevel (Either ParError (a, [ErrorMsg]), ParState)

instance Alternative Parser where
    empty = Parser $ throwError ParError
    Parser ma <|> Parser mb = Parser $ catchError ma $
        \e -> gets consumed >>= \p -> if p then throwError e else mb

flipConsumed :: Parser ()
flipConsumed = modify $ \s -> s { consumed = True }

peekToken :: Parser Token
peekToken = do
    s <- gets stream
    case s of
        (x:xs) -> return $ fst x
        _ -> throwError ParError

peekSpan :: Parser Span
peekSpan = do
    s <- gets stream
    case s of
        (x:xs) -> return $ snd x
        _ -> throwError ParError

nextToken :: Parser Token
nextToken = do
    s <- gets stream
    case s of
        (x:xs) -> do
            modify $ \s -> s { stream = xs , consumed = True }
            return (fst x)
        _ -> throwError ParError

token :: Token -> Parser ()
token tok = do
    tok' <- peekToken
    if tok == tok' then void nextToken
    else throwError ParError

token' :: (Token -> Maybe a) -> Parser a
token' f = do
    tok <- peekToken
    case f tok of
        Just a -> nextToken >> return a
        Nothing -> throwError ParError

ident :: Parser InternStr
ident = token' unIdent

name :: Parser Name
name = fmap Var ident

int :: Parser Int
int = token' unInt

real :: Parser Double
real = token' unReal

bool :: Parser Bool
bool = token' unBool

char :: Parser Char
char = token' unChar

withParens :: Parser a -> Parser a
withParens par = token LParen *> par <* token RParen


lExpr :: Parser LExpr
lExpr = do
    peekToken >>= \case
        TokInt _ -> LInt <$> int
        TokReal _ -> LReal <$> real
        TokBool _ -> LBool <$> bool
        TokChar _ -> LChar <$> char
        TokId _ -> LVar <$> name
        Fn -> lLam
        LParen -> withParens lApp
        _ -> throwError ParError

lLam :: Parser LExpr
lLam = do
    token Fn
    args <- many name
    token EArrow
    body <- lExpr
    return $ foldr LLam body args

lApp :: Parser LExpr
lApp = do
    exprs <- some lExpr
    return $ foldl1 LApp exprs




{-
bind :: String -> Parser a -> Parser a
bind x = local (x :)

inEnv :: String -> Parser ()
inEnv x = do
    xs <- ask
    guard (x `elem` xs)


type Parser a = Parsec [TokenPos] () a

-- | Parse the given nullary token
tok :: Token -> Parser ()
tok t = tokenPrim showTok nextPos testTok
    where
        showTok = show
        nextPos pos x xs = _end x
        testTok x = if _token x == t then Just () else Nothing

tok' :: (Token -> Maybe a) -> Parser a
tok' f = tokenPrim showTok nextPos testTok
    where
        showTok = show
        nextPos pos x xs = _end x
        testTok x = f (_token x)

eInt :: Parser LExpr
eInt = do
    x <- tok' unInt
    return $ LInt x

eVar :: Parser LExpr
eVar = do
    x <- tok' unIdent
    return $ LVar x

eLam :: Parser LExpr
eLam = do
    tok Fn
    args <- many1 $ tok' unIdent
    tok EArrow <?> "EArrow"
    body <- eApp <?> "body of lam"
    return $ LLam args body

eApp :: Parser LExpr
eApp = do
    func <- expr <?> "function for app"
    args <- many expr <?> "argument for app"
    if null args then return func
    else return $ case func of
        (LVar x) -> case opMap x of
            Just op -> LOpr op args
            Nothing -> LApp func args
        _ -> LApp func args


eRecord :: Parser LExpr
eRecord = do
    tok LBracket
    xs <- sepBy1 expr (tok Comma)
    tok RBracket
    return $ LRecord xs

eSelect :: Parser LExpr
eSelect = do
    tok LBrace
    x <- expr
    tok Colon
    i <- tok' unInt
    tok RBrace
    return $ LSelect i x


opMap :: Name -> Maybe Opr
opMap "add" = Just IAdd
opMap "sub" = Just ISub
opMap "mul" = Just IMul
opMap "div" = Just IDiv
opMap _ = Nothing


expr :: Parser LExpr
expr = choice
    [ eInt
    , eVar
    , eLam
    , tok LParen *> eApp <* tok RParen
    , eRecord
    , eSelect
    ]

parseExpr :: String -> Either ParseError LExpr
parseExpr str = do
    toks <- tokenize "input" str
    -- trace (concatMap show toks) (return ())
    runParser expr () "input" toks


-}