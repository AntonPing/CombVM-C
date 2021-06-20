module Combinator where
import Prelude
import Debug.Trace ( trace )

data LTerm =
      LVar String
    | LAbs String LTerm
    | LApp LTerm LTerm
    deriving Show

data CTerm =
      CVar String
    | S | K | I
    | CApp CTerm CTerm
    deriving Show



data ITerm =
      IVar String
    | IAbs String ITerm
    | IS | IK | II
    | IApp ITerm ITerm

class Term a where
    liftTerm :: a -> ITerm


freeVar :: LTerm -> [String]
freeVar (LVar x) = [x]
freeVar (LAbs x t) = filter (/= x) (freeVar t)
freeVar (LApp t1 t2) = freeVar t1 ++ freeVar t2

change :: String -> LTerm -> LTerm -> LTerm
change v t (LVar x)
    | x == v = t
    | x /= v = LVar x
change v t (LAbs x t')
    | x == v = LAbs x t'
    | x /= v = LAbs x (change v t t')
change v t (LApp t1 t2) = 
    LApp (change v t t1) (change v t t2)

newFree :: String -> LTerm -> String
newFree v t = until esc iter v where
    esc v1 = v1 `notElem` freeVar t
    iter v2 = v2 ++ "'"

normal :: LTerm -> Bool
normal (LVar _) = True
normal (LAbs _ _) = True
normal (LApp (LVar _) _) = True
normal (LApp (LAbs _ _) _) = False
normal (LApp t@(LApp _ _) _) = normal t

eval :: LTerm -> LTerm
eval t@(LVar _) = trace "1!" t
eval t@(LAbs _ _) = trace "2!" t
eval t@(LApp (LVar _) _) = trace "3!" t
eval (LApp (LAbs x t1) t2) =
    if null [ t | t <- freeVar t1, t `elem` freeVar t2]
        then trace ("apply! " ++ x ++ " -> " ++ show t2) 
            (change x t2 t1)
        else trace "alpha!" (change newVar t2 newTerm)
        where
            newVar = newFree x t1 
            newTerm = change x (LVar newVar) t1
eval (LApp t1@(LApp _ _) t2) =
    if normal t1
    then LApp t1 t2
    else eval (LApp (eval t1) t2)


isFree :: String -> ITerm -> Bool
isFree v (IVar x) = x /= v
isFree v (IAbs _ t) = isFree v t
isFree v (IApp t1 t2) = isFree v t1 && isFree v t2

c2l :: CTerm -> LTerm
c2l (CVar x) = LVar x
c2l S = LAbs "f" $ LAbs "g" $ LAbs "x" $ LApp 
    (LApp (LVar "f") (LVar "x"))
    (LApp (LVar "g") (LVar "x"))
c2l K = LAbs "x" $ LAbs "y" $ LVar "x"
c2l I = LAbs "x" $ LVar "x"
c2l (CApp t1 t2) = LApp (c2l t1) (c2l t2)

l2c :: LTerm -> CTerm
l2c (CVar x) = LVar x
l2c LAbs "f" $ LAbs "g" $ LAbs "x" $ LApp 
    (LApp (LVar "f") (LVar "x"))
    (LApp (LVar "g") (LVar "x")) = S

l2c (LAbs "x" $ LAbs "y" $ LVar "x") = I
 K = LAbs "x" $ LAbs "y" $ LVar "x"
l2c (LAbs "x" $ LVar "x") = C
c2l (CApp t1 t2) = LApp (c2l t1) (c2l t2)





l2i :: LTerm -> ITerm
l2i (LVar x) = IVar x
l2i (LAbs x t) = IAbs x $ l2i t
l2i (LApp t1 t2) = IApp


i2i :: ITerm -> ITerm
i2i (IVar x) = IVar x
i2i (IAbs x t)
    | isFree x t = IApp IK (i2i t)
    | not (isFree x t) = case t of
        IVar x' -> II -- x' must equal to x, 'Cause x is not free in t
        IAbs x' t' -> i2i $ IAbs x $ i2i $ IAbs x' t'
        IApp t1 t2 -> IApp (IApp IS (i2i t1)) (i2i t2)
i2i (IApp t1 t2) = IApp (i2i t1) (i2i t2)


swap = LAbs "x" (LAbs "y" (LApp (LVar "y") (LVar "x")))
test = LApp swap (LVar "a")
test3 = LApp (LApp swap (LVar "a")) (LVar "b")
ident = LAbs "x" (LVar "x")

test2 = LApp ident (LVar "y")

main = print (eval test)



