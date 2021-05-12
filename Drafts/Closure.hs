import Prelude
import Data.List
import System.IO
import Debug.Trace

data Term =
      Var String
    | Abs String Term
    | App Term Term
    | Env String Term
    | Closure Term
    deriving (Eq)

instance Show Term where
    show (Var x) = x
    show (Abs x t) = "λ." ++ x ++ "->" ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Env x t) = x ++ "=" ++ show t
    show (Closure (App t1 t2)) = "{" ++ show t1 ++ " " ++ show t2 ++ "}"
    show (Closure t) = "{" ++ show t ++ "}"

rewind :: [Term] -> Term
rewind (x:xs) =  foldl App x xs

evalShow :: Term -> Term
evalShow t = trace (show t) $ eval t

evalLoop :: Term -> Term
evalLoop t = 
    let t' = evalShow t in
    if t == t' then t
    else evalLoop t' 

eval :: Term -> Term
eval (Var x) = Var x
eval (Abs x t) = Abs x t
eval (Env y z) = Env y z
eval (Closure t) = Closure (eval t)
eval (App (Abs x t) t2) =
    App (Env x t2) t
-- about Environment
eval (App (Env y z) (Var x)) =
    if x == y then z
    else Var x
eval (App (Env y z) (Abs x t)) =
    if x == y then Abs x t
    else Abs x (App (Env y z) t)
eval (App (Env y z) (App (Env y' z') t2)) =
    App (Env y z) (eval $ App (Env y' z') t2)
eval (App (Env y z) (App t1 t2)) =
    let new_t1 = App (Env y z) t1
        new_t2 = App (Env y z) t2 in
    App new_t1 new_t2
-- about Closure
eval (App (Env _ _) (Closure t)) =
    Closure t
eval (App (Closure t1) (Closure t2)) =
    Closure (App t1 t2)
eval (App (Closure t1) t2) =
    Closure (App t1 t2)
-- other situation
eval (App t1 t2) =
    let try1 = eval t1
        try2 = eval t2 in
    if try1 /= t1
    then App try1 t2
    else if try2 /= t2
    then App t1 try2
    else App t1 t2

{-
makeApp :: [Term] -> Term
makeApp [] = error "empty"
makeApp [x] = x
makeApp (x:y:r) = makeApp (App x y:r)
-}

test2 = Abs "x" $ Abs "y" $ App (Var "y") (Var "x")
test3 = App (App test2 (Var "z")) (Var "w")
test = evalLoop test3