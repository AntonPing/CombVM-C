module Utils.Syntax where

import qualified Data.Text as T

-- import Utils.Symbol (Name)

type Name = String

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

isPurePrim :: Prim -> Bool
isPurePrim _ = True
-- all prims all pure so far

data BrsPrim
    = Switch
    | Halt
    deriving (Eq)

data HelpTag
    = HelpTag
    deriving (Eq)



data Def = Def
    { defFunc :: Name
    , defArgs :: [Name]
    , defBody :: CExpr
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
    
{-
data LExpr
    = LInt Int
    | LReal Double
    | LVar Name
    | LLam [Name] LExpr
    | LApp LExpr [LExpr]
    | LOpr Opr [LExpr]
    | LRecord [LExpr]
    | LSelect Int LExpr
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

data Opr
    = IAdd
    | ISub
    | IMul
    | IDiv
    | INeg
    | BAnd
    | BOr
    | BNot
    | RegSwap
    deriving (Eq)

type Def = (Name,[Name],CExpr)

data AccessPath
    = PathOff Int Atom
    | PathSel Int AccessPath
    deriving (Eq)

data CExpr
    = CApp Atom [Atom]
    | CLet Def CExpr
    | CFix [Def] CExpr
    | CRecord [AccessPath] Name CExpr
    | CSelect Int Atom Name CExpr
    -- | COffset Int Atom Name CExpr
    | CBinop Opr Atom Atom Name CExpr
    | CUniop Opr Atom Name CExpr
    | CIfte Atom CExpr CExpr
    | CSwitch Atom [CExpr]
    | CHalt Atom
    -- an intermidate node for optmization
    | Tag HelpTag CExpr
    deriving (Eq)

data HelpTag 
    = SubstAtom Name Atom
    | SubstFunc (Name,[Name],CExpr)
    | BindTimes Int
    | FreeVar [Name]

    deriving (Eq)


-}