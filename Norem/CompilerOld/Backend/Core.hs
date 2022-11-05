module Backend.Core where

import Utils.Symbol ( Name )

{-

data LExpr
    = LInt Int
    | LReal Double
    | LChar Char
    | LBool Bool
    | LVar Name
    | LLam Name LExpr
    | LApp LExpr LExpr
    | LOpr Prim [LExpr]
    | LBrs BrsPrim [LExpr] [LExpr]
    | LRec [LExpr]
    | LGet Int LExpr
    deriving (Eq)

data Atom
    = AVar Name
    | ALabel Name
    | AReg Int
    | AInt Int
    | AReal Double
    | ABool Bool
    | AChar Char
    deriving (Eq)

data Prim
    = IAdd
    | ISub
    | IMul
    | IDiv
    | INeg
    | BAnd
    | BOr
    | BNot
    deriving (Eq)

data BrsPrim
    = Switch
    deriving (Eq)

data HelpTag
    = HelpTag
    deriving (Eq)

data Def = Def
    { func :: Name
    , args :: [Name]
    , body :: CExpr
    } deriving (Eq)

data CExpr =
      CApp Atom [Atom]
    | CLet [Def] CExpr
    | COpr Prim [Atom] [Name] CExpr
    | CBrs BrsPrim [Atom] [CExpr]
    -- record[i] -> r in c
    | CRec Int Name CExpr
    -- get r[i] -> x in c
    | CGet Atom Int Name CExpr
    -- set r[i] := x in c
    | CSet Atom Int Atom CExpr
    -- an intermidate node for optmization
    | Tag HelpTag CExpr
    deriving (Eq)
-}