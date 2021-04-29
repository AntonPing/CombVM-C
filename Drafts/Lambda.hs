import Prelude
import Data.List
import System.IO
import Debug.Trace

data Term =
      Var String
    | Abs String Term
    | App Term Term
    | Closure Term
    | Bind [(String,Term)] Term
    
instance Show Term where
    show (Var x) = x
    show (Abs x t) = "λ." ++ x ++ "->" ++ show t
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Closure (App t1 t2)) = "{" ++ show t1 ++ " " ++ show t2 ++ "}"
    show (Closure t) = "{" ++ show t ++ "}"
    show (Bind xs t) = "[" ++ showBind xs ++ ": " ++ show t ++ "]"

showBind :: [(String,Term)] -> String
showBind (x:xs) =
    let showPair (k,v) = k ++ "=" ++ show v in
        foldl (\x y -> x ++ "," ++ showPair y) (showPair x) xs


rewind :: [Term] -> Term
rewind (x:xs) =  foldl App x xs

evalShow :: Term -> [Term] -> Term
evalShow t stk = trace (show $ rewind (t:stk)) eval t stk

eval :: Term -> [Term] -> Term
eval t@(Var _) stk = rewind (t:stk)
eval t@(Abs _ _) [] = t
eval (Abs x t) (top:rst) =
    evalShow (Bind [(x,top)] t) rst
eval (App t1 t2) stack = evalShow t1 (t2:stack)
eval (Closure t) stack = eval t stack
eval (Bind lst (Var x)) stack = 
    case lookup x lst of
        Just result -> evalShow result stack
        Nothing -> evalShow (Var x) stack
eval (Bind lst (Abs x t)) (top:rst) =
    evalShow (Bind ((x,top):lst) t) rst
eval (Bind lst (App t1 t2)) stk =
    evalShow (Bind lst t1) (Bind lst t2:stk)
eval (Bind lst (Closure t)) stk =
    evalShow t stk
eval (Bind lst (Bind lst' t)) stk =
    eval (Bind (lst++lst') t) stk

makeApp :: [Term] -> Term
makeApp [] = error "empty"
makeApp [x] = x
makeApp (x:y:r) = makeApp (App x y:r)

test2 = Abs "x" $ Abs "y" $ App (Var "y") (Var "x")
test3 = App (App test2 (Var "z")) (Var "w")

test = eval test3 []