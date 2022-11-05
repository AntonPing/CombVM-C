module Frontend.Lexer where

import Control.Monad

import Frontend.Token as Tok

import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
    ( ExceptT(..), runExceptT, MonadError(..), catchError )

import Data.Maybe
import Debug.Trace
import Data.Char (isSpace, isAlphaNum, isAlpha, isDigit)
import Text.Read (readMaybe)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Applicative
import Data.Either (fromRight)
import Utils.TopLevel (TopLevel, MonadTopLevel (localEnv), askEnv, intern)
import qualified Data.Map as M
import Control.Monad.Cont


data LexError = LexError
    deriving (Eq,Show)

data LexState = LexState
    { stream :: String
    , currPos :: Position
    -- , layout :: Int
    } deriving (Eq,Show)

{-
type Lexer = ExceptT LexError (State LexState)

many :: MonadError LexError m => m a -> m [a]
many m = catchError ((:) <$> m <*> many m) (\e -> return [])

some :: MonadError LexError m => m a -> m [a]
some m = mant m <* 
-}

newtype Lexer a = Lexer
    { unLexer :: ExceptT LexError (StateT LexState TopLevel) a }
    deriving 
        ( Functor, Applicative, Monad
        , MonadError LexError
        , MonadState LexState
        , MonadTopLevel
        , MonadIO
        )

runLexer :: Lexer a -> LexState -> TopLevel (Either LexError a, LexState)
runLexer (Lexer m) = (runStateT . runExceptT) m

evalLexer :: Lexer a -> LexState -> TopLevel (Either LexError a)
evalLexer (Lexer m) = (evalStateT . runExceptT) m

mkLexState :: String -> LexState
mkLexState str = LexState { stream = str, currPos = zeroPos }

runLexerStr :: Lexer a -> String -> TopLevel (Either LexError a, LexState)
runLexerStr m str = runLexer m (mkLexState str)

evalLexerStr :: Lexer a -> String -> TopLevel (Either LexError a)
evalLexerStr m str = evalLexer m (mkLexState str)

instance Alternative Lexer where
    empty = Lexer $ throwError LexError
    Lexer ma <|> Lexer mb = Lexer $ catchError ma (const mb)

movePos :: Char -> Position -> Position
movePos '\n' Position { row, col, abs } = Position
    { row = row + 1
    , col = 0
    , abs = abs + 1
    }
movePos _ Position { row, col, abs } = Position
    { row = row
    , col = col + 1
    , abs = abs + 1
    }

peekChar :: Lexer Char
peekChar = do
    s <- gets stream
    case s of
        (x:xs) -> return x
        _ -> throwError LexError

nextChar :: Lexer Char
nextChar = do
    s <- gets stream
    case s of
        (x:xs) -> do
            pos <- gets currPos
            put $ LexState { stream = xs, currPos = movePos x pos }
            return x
        _ -> throwError LexError

peekEof :: Lexer ()
peekEof = do
    s <- gets stream
    case s of
        [] -> return ()
        _ -> throwError LexError

isEof :: Lexer Bool
isEof = do
    s <- gets stream
    case s of
        [] -> return True
        _ -> return False

satisfy :: (Char -> Bool) -> Lexer Char
satisfy pred = do
    ch <- peekChar
    guard $ pred ch
    nextChar

char :: Char -> Lexer Char
char ch = satisfy (== ch)

space :: Lexer Char
space = satisfy isSpace

digit :: Lexer Char
digit = satisfy isDigit

alpha :: Lexer Char
alpha = satisfy isAlpha

alphaNum :: Lexer Char
alphaNum = satisfy isAlphaNum

lineComment :: Lexer ()
lineComment = void $ many $ satisfy (/= '\n')

blockComment ::  Int -> Lexer ()
blockComment 0 = return ()
blockComment n = do
    nextChar >>= \case
        '{' -> peekChar >>= \case
            '-' -> nextChar >> blockComment (n + 1)
            _ -> blockComment n
        '-' -> peekChar >>= \case
            '}' -> nextChar >> blockComment (n - 1)
            _ -> blockComment n
        _ -> blockComment n 

lexErrorToken :: Lexer Token
lexErrorToken = do
    many $ satisfy (not . isSpace)
    return ErrorToken

nextTokenSpan :: Lexer (Token,Span)
nextTokenSpan = do
    p <- isEof
    when p $ throwError LexError
    start <- gets currPos
    token <- flip catchError (\e -> lexErrorToken) nextToken
    end <- gets currPos
    return (token, Span start end)

keywordMap :: M.Map String Token
keywordMap = M.fromList
    [ ("fn", Fn)
    , ("if", If)
    , ("then", Then)
    , ("else", Else)
    ]

nextToken :: Lexer Token
nextToken =  do
    many space
    nextChar >>= \case
        '(' -> return LParen
        ')' -> return RParen
        '[' -> return LBracket
        ']' -> return RBracket
        '{' -> peekChar >>= \case
            '-' -> nextChar >> blockComment 1 >> nextToken
            _ -> return LBrace
        '}' -> return RBrace
        ',' -> return Comma
        '.' -> return Dot
        ':' -> return Colon
        ';' -> return Semicolon
        '|' -> return Bar
        '_' -> return Wild
        '-' -> peekChar >>= \case
            '>' -> nextChar >> return Arrow
            '-' -> nextChar >> lineComment >> nextToken
            _ -> TokOpr <$> intern "-"
        '=' -> peekChar >>= \case
            '>' -> nextChar >> return EArrow
            _ -> TokOpr <$> intern "="
        ch | isAlpha ch -> do
            rest <- many alphaNum
            case M.lookup (ch:rest) keywordMap of
                Just tok -> return tok
                Nothing -> TokId <$> intern (ch:rest)
        ch | isDigit ch -> do
            rest <- many digit
            case readMaybe (ch:rest) of
                Just n -> return $ TokInt n
                Nothing -> throwError LexError
        _ -> throwError LexError


tokenize :: Lexer [(Token,Span)]
tokenize = many nextTokenSpan <* peekEof






{-
lexerTest1 :: String -> StateInternMap [(Token, Span)]
lexerTest1 str = fromRight [] <$> evalLexer tokenize (mkLexState str)

lexerTest2 :: String -> [(Token,Span)]
lexerTest2 = flip evalStateInternMap emptyInternMap . lexerTest1

lexerTestIO :: String -> IO ()
lexerTestIO str = do
    putStrLn str
    let stream = lexerTest2 str
    print stream
-}
-- lexerTest2 :: String -> 

--runTokenize :: MonadStateInternMap m => String -> m (Either LexError [(Token,Span)])
--runTokenize str = fst $ runLexer tokenize (mkLexerState str)


{-
type Lexer = State LexInput

makeInput :: String -> LexInput
makeInput s = LexInput 
    { stream = s
    , _row = 0
    , _col = 0
    , _abs = 0
    } 

getPosition :: (MonadState LexInput m) => m Position
getPosition = do
    LexInput { _row, _col, _abs, ..} <- get
    return $ Position { row = _row, col = _row, abs = _abs }

movePos :: (MonadState LexInput m) => Char -> m ()
movePos '\n' = modify' $ \inp -> inp
    { _row = _row inp + 1
    , _col = 0
    , _abs = _abs inp + 1
    }
movePos ch = modify' $ \inp -> inp
    { _col = _col inp + 1
    , _abs = _abs inp + 1
    }

peekChar :: (MonadState LexInput m) => m (Maybe Char)
peekChar = do
    s <- gets stream
    case s of
        (x:xs) -> return $ Just x
        _ -> return Nothing
{-

-}

nextChar :: (MonadState LexInput m) => m (Maybe Char)
nextChar = do
    s <- gets stream
    case s of
        (x:xs) -> do
            modify' $ \inp -> inp{ stream = xs }
            movePos x
            return $ Just x
        _ -> return Nothing

nextSatisfy :: (MonadState LexInput m) => (Char -> Bool) -> m (Maybe Char)
nextSatisfy f = do
    s <- gets stream
    case s of
        (x:xs)
            | f x -> do
                modify' $ \inp -> inp{ stream = xs }
                movePos x
                return $ Just x
            | otherwise -> return Nothing
        _ -> return Nothing

foreverWhile :: (Monad m) => m (Maybe a) -> m [a]
foreverWhile m = do
    res <- m
    case res of
        Just a -> (a:) <$> foreverWhile m
        Nothing -> return []

nextWhile :: (MonadState LexInput m) => (Char -> Bool) -> m String
nextWhile f = foreverWhile $ nextSatisfy f

nextUntil :: (MonadState LexInput m) => (Char -> Bool) -> m String
nextUntil f = foreverWhile $ nextSatisfy (not . f)

nextTokenSpan :: (MonadState InternMap m, MonadState LexInput m) => m (Token,Span)
nextTokenSpan = do
    start <- getPosition
    token <- nextToken
    end <- getPosition
    return (token, Span start end)

lexErrorToken :: (MonadState LexInput m) => m Token
lexErrorToken = do
    nextUntil isSpace
    return ErrorToken

nextToken :: (MonadState InternMap m, MonadState LexInput m) => m Token
nextToken = do
    nextWhile isSpace
    first <- nextChar
    case first of
        Just '(' -> return LParen
        Just ')' -> return RParen
        Just '[' -> return LBracket
        Just ']' -> return RBracket
        Just '{' -> do
            second <- peekChar
            case second of
                Just '-' -> nextChar >> blockComment 1
                _ -> return LBrace
        Just '}' -> return RBrace
        Just ',' -> return Comma
        Just '.' -> return Dot
        Just ':' -> return Colon
        Just ';' -> return Semicolon
        Just '|' -> return Bar
        Just '_' -> return Wild
        Just '-' -> do
            second <- peekChar
            case second of
                Just '>' -> nextChar >> return Arrow
                Just '-' -> nextChar >> lineComment >> nextToken
                _ -> TokOpr <$> intern "-"
        Just '=' -> do
            second <- peekChar
            case second of
                Just '>' -> nextChar >> return EArrow
                _ -> TokOpr <$> intern "="
        Just ch | isAlpha ch -> do
            rest <- nextWhile isAlphaNum
            TokId <$> intern (ch:rest)
        Just ch | isDigit ch -> do
            rest <- nextWhile isDigit
            case readMaybe (ch:rest) of
                Just n -> return $ TokInt n
                Nothing -> lexErrorToken
        Just _ -> lexErrorToken
        Nothing -> return EndOfFile


lineComment :: (MonadState LexInput m) => m ()
lineComment = void $ nextUntil (== '\n')

blockComment :: (MonadState InternMap m, MonadState LexInput m) => Int -> m Token
blockComment 0 = nextToken
blockComment n = do
    first <- nextChar
    case first of
        Just '{' -> do
            second <- peekChar
            case second of
                Just '-' -> nextChar >> blockComment (n + 1)
                Just _ -> blockComment n
                Nothing -> return ErrorToken
        Just '-' -> do
            second <- peekChar
            case second of
                Just '}' -> nextChar >> blockComment (n - 1)
                Just _ -> blockComment n
                Nothing -> return ErrorToken
        Just _ -> blockComment n
        Nothing -> return ErrorToken


{-
char :: Lexer ()
char = do
    ask 
-}

{-

blockComment :: Lexer ()
blockComment = choice
    [ void $ try (string "")
    , void $ try (string "") >> blockComment >> blockComment
    , anyChar >> blockComment
    ]

lineComment :: Lexer ()
lineComment = void $ manyTill anyChar (eof <|> void newline)

tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser ((catMaybes <$> many lexTokPosMaybe) <* eof) ()

lexTokPosMaybe :: Lexer (Maybe TokenPos)
lexTokPosMaybe = choice
    [ Nothing <$ (space >> spaces)
    , Nothing <$ try (string "" >> blockComment)
    , Nothing <$ try (string "--" >> lineComment)
    , Just <$> lexTokPos
    ]

lexTokPos :: Lexer TokenPos
lexTokPos = do 
    start <- getPosition;
    tok <- lexTok;
    end <- getPosition;
    trace (show tok) (return ())
    return $ TokenPos tok start end;

-- | Lex one non-alphanumeric token
lexTok :: Lexer Token
lexTok = choice 
    [ LParen  <$  char '('
    , RParen  <$  char ')'
    , LBracket  <$  char '['
    , RBracket  <$  char ']'
    , LBrace  <$  char '{'
    , RBrace  <$  char '}'
    , Comma <$  char ','
    , Dot <$  char '.'
    , Colon <$  char ':'
    , Semicolon <$  char ';'
    , Bar <$  char '|'
    , Wild <$  char '_'
    , Arrow <$ try (string "->")
    , EArrow <$ try (string "=>")
    , Equal <$ char '='
    , Fn  <$  try (string "fn")
    , If  <$  try (string "if")
    , Then  <$  try (string "then")
    , Else  <$  try (string "else")
    , Ident <$> try lexIdent
    , TokInt <$> try lexInt
    ]

lexIdent :: Lexer String
lexIdent = (:) <$> letter <*> many (alphaNum <|> char '-')

lexInt :: Lexer Int
lexInt = fromInteger <$> Parsec.integer haskell

lexReal :: Lexer Double
lexReal =  Parsec.float haskell

lexBool :: Lexer Bool
lexBool = True <$ try (string "true")
    <|> False <$ try (string "false")

-}

-}