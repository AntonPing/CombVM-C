module Frontend.Token where

import Text.Parsec ( SourcePos )
import Utils.Symbol (Name)
import Utils.TopLevel (InternStr)

{-
data TokenPos = TokenPos
    { token :: Token
    , start :: SourcePos
    , end :: SourcePos
    } deriving (Eq,Show)
-}

data Position = Position
    { row :: Int
    , col :: Int
    , abs :: Int
    } deriving (Eq,Show)

zeroPos :: Position
zeroPos = Position
    { row = 0
    , col = 0
    , abs = 0
    }

data Span = Span
    { start :: Position
    , end :: Position
    } deriving (Eq,Show)

data Token =
      LParen
    | RParen
    | LBracket
    | RBracket
    | LBrace
    | RBrace

    | Comma
    | Dot
    | Colon
    | Semicolon
    | Bar
    | Wild
    | Equal
    
    | EArrow
    | Arrow
    | Fn
    | If
    | Then
    | Else
    
    | TokId InternStr
    | TokOpr InternStr
    | TokInt Int
    | TokReal Double
    | TokBool Bool
    | TokChar Char
    | TokStr InternStr

    | StartOfFile
    | EndOfFile
    | ErrorToken
    deriving (Eq,Show)

unIdent :: Token -> Maybe InternStr
unIdent (TokId x) = Just x
unIdent _ = Nothing

unOpr :: Token -> Maybe InternStr
unOpr (TokOpr x) = Just x
unOpr _ = Nothing

unInt :: Token -> Maybe Int
unInt (TokInt x) = Just x
unInt _ = Nothing

unReal :: Token -> Maybe Double
unReal (TokReal x) = Just x
unReal _ = Nothing

unBool :: Token -> Maybe Bool
unBool (TokBool x) = Just x
unBool _ = Nothing

unChar :: Token -> Maybe Char
unChar (TokChar x) = Just x
unChar _ = Nothing

unStr :: Token -> Maybe InternStr
unStr (TokStr s) = Just s
unStr _ = Nothing