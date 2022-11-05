{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import Utils
import Prettyprinter.Render.Text hiding (Show)
import Prettyprinter hiding (Show)
import qualified Data.Text as T

bracketed :: [Doc ann] -> Doc ann
bracketed =  encloseSep "(" ")" " "

docRender :: Doc ann -> T.Text
docRender = renderStrict . layoutPretty defaultLayoutOptions

instance Show Atom where
    show = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty

instance Show Opr where
    show = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty

instance Show LExpr where
    show = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty

instance Show CExpr where
    show = T.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty


instance Pretty Atom where
    pretty (AVar x) = pretty x
    pretty (ALabel x) = pretty x
    pretty (AReg x) = "$r" <> pretty x
    pretty (AInt x) = pretty x
    pretty (AReal x) = pretty x
    pretty (ABool x) = pretty x
    pretty (AChar x) = pretty x
    
instance Pretty Opr where
    pretty IAdd = "iAdd"
    pretty ISub = "iSub"
    pretty IMul = "iMul"
    pretty IDiv = "iDiv"
    pretty INeg = "iNeg"
    pretty BAnd = "bAnd"
    pretty BOr = "bOr"
    pretty BNot = "bNot"
    pretty RegSwap = "regSwap"


instance Pretty LExpr where
    pretty (LInt x) = pretty x
    pretty (LReal x) = pretty x
    pretty (LVar x) = pretty x
    pretty (LLam args body) = 
        "(fn" <+> sep (map pretty args) <+> "=>"
            <+> pretty body <> ")"
    pretty (LApp func args) = 
        encloseSep "(" ")" " " (map pretty (func:args))
    pretty (LOpr op args) = 
        encloseSep "(" ")" " " (pretty op : map pretty args)
    pretty (LRecord xs) = 
        encloseSep "[" "]" "," (map pretty xs)
    pretty (LSelect i x) =
        "{" <+> pretty x <+> ":" <+> pretty i <+> "}"

prettyDef (func,args,body) = 
    sep (map pretty (func:args)) <+> "=" <> hardline <> indent 2 (pretty body)



instance Pretty AccessPath where
    pretty (PathOff 0 x) = pretty x
    pretty (PathOff n x) = "&" <> pretty x <> "[" <> pretty n <> "]"
    pretty (PathSel n p) = pretty p <> "[" <> pretty n <> "]"

instance Pretty CExpr where
    pretty (CApp func args) =
        encloseSep "(" ")" " " (map pretty (func:args))
    pretty (CLet def body) =
        "let" <+> prettyDef def <+> "in" <> hardline <> pretty body
    pretty (CFix defs body) = 
        "letrec" <> hardline <>  (indent 2 . sep) (map prettyDef defs) <+>
        "in" <+> hardline <+> pretty body
    pretty (CUniop op x z k) = 
        pretty z <+> "<-" <+> pretty op <+> pretty x
        <> hardline <> pretty k
    pretty (CBinop op x y z k) = 
        pretty z <+> "<-" <+> pretty op <+> pretty x <+> pretty y
        <> hardline <> pretty k
    pretty (CRecord xs y k) = 
        pretty y <+> "<-" <+> encloseSep "[" "]" "," (map pretty xs)
        <> hardline <> pretty k
    pretty (CSelect i x y k) =
        pretty y <+> "<-" <+> "@" <> pretty i <> "(" <> pretty x <> ")"
        <> hardline <> pretty k
    pretty (CSwitch i xs) =
        "switch" <+> pretty i <+> "of"
        <> hardline <> (indent 2 . sep) (map pretty xs)
    {-
    pretty (COffset i x y k) = 
        pretty y <+> "<-" <+> "offset" <+> pretty i <+> pretty x
        <> hardline <> pretty k
    -}
    pretty (CIfte cond trbr flbr) = 
        "if" <+> pretty cond <> softline <>
        "then" <+> pretty trbr <> softline <>
        "else" <+> pretty flbr
    pretty (CHalt x) =
        "halt" <+> pretty x
    pretty (Tag tg e) = "{" <> pretty tg <+> "|" <+> pretty e <+> "}"

instance Pretty HelpTag where
    pretty (SubstAtom x y) =
        "SubstAtom" <+> pretty x <+> ":=" <+> pretty y
    pretty (SubstFunc (func,args,body)) =
        "SubstFunc" <+> pretty (func,args,body)
    pretty (BindTimes n) =
        "BindTimes" <+> pretty n <+> "times"
    pretty (FreeVar xs) =
        "FreeVar" <+> sep (map pretty xs)