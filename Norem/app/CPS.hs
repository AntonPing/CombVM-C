{-# LANGUAGE OverloadedStrings #-}

module CPS where

import Utils
import Control.Effect.State
import Control.Effect.Cont
import Data.Functor.Identity

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.MultiSet as MS

import Debug.Trace (trace)

import Pretty
import Data.Maybe (fromJust, fromMaybe)

import Data.Functor.Base (TreeF(..))
import Data.Functor.Foldable
import Data.Bifunctor


genvar :: ContT CExpr (State Integer) Name
genvar = do
    n <- get
    put $ n + 1
    return $ T.pack $ "#" ++ show n

type CpsTrans = ContT CExpr (State Integer) Atom

colift :: Cont (r,s) a -> ContT r (State s) a
-- (a -> r) -> r => (a -> m r) -> m r
colift c = ContT $ \amr -> state $ \s -> runCont c ((`runState` s) . amr)




-- thanks AQ


--colift2 :: ((a -> s -> (r,s)) -> (s -> (r,s))) -> ContT r (State Integer) a
--colift2 arr = Cont $ \amr -> 


{-

colift2 :: Cont r a -> ContT r (State Integer) a
-- (a -> r) -> r => (a -> m r) -> m r
-- (a -> r) -> r => (a -> (s -> (r,s))) -> (s -> (r,s))
colift2 arr0 = ContT $ \amr -> \a -> runState 0 (amr a)
    where
        arr = runCont arr0
        amrmr = \amr 

colift3 :: ((a -> r) -> r) -> (a -> (Integer -> (r,Integer))) -> Integer -> (r,Integer)
colift3 arr asrs s = 
    let ar = \a -> fst (asrs a s) 
        as = \a -> snd (asrs a s) 
    in
    (arr ar, s)
-}

cpsTransTop :: LExpr -> CExpr
cpsTransTop expr = evalState
    (runContT (cpsTrans expr) (return . CHalt)) 0


cpsTrans :: LExpr -> CpsTrans
cpsTrans (LVar x) = return (AVar x)
cpsTrans (LInt x) = return (AInt x)
cpsTrans (LReal x) = return (AReal x)
cpsTrans (LOpr op [x]) = do
    x' <- cpsTrans x
    new <- genvar
    ContT $ \k -> do
        res <- k (AVar new)
        return $ CUniop op x' new res
cpsTrans (LOpr op [x,y]) = do
    x' <- cpsTrans x
    y' <- cpsTrans y
    new <- genvar
    ContT $ \k -> do
        res <- k (AVar new)
        return $ CBinop op x' y' new res
cpsTrans (LOpr op _) = error "??"
cpsTrans (LApp func args) = do
    func' <- cpsTrans func
    args' <- mapM cpsTrans args
    newk <- genvar
    new <- genvar
    ContT $ \k -> do
        res <- k (AVar new)
        return $ CLet (newk,[new],res) (CApp func' (AVar newk:args'))
cpsTrans (LLam args body) = do
    f <- genvar
    c <- genvar
    n <- get
    -- fresh k should be generated!
    let (body', n') = runState (runContT (cpsTrans body)
            (return . \z -> CApp (AVar c) [z])) n
    put n'
    ContT $ \k -> do
        res <- k (AVar f)
        return $ CLet (f,c:args,body') res
cpsTrans (LRecord xs) = do
    xs' <- mapM cpsTrans xs
    new <- genvar
    ContT $ \k -> do
        res <- k (AVar new)
        return $ CRecord (map (PathOff 0) xs') new res
cpsTrans (LSelect i x) = do
    x' <- cpsTrans x
    new <- genvar
    ContT $ \k -> do
        res <- k (AVar new)
        return $ CSelect i x' new res








{-
betaReduce :: CExpr -> State [Name] CExpr
betaReduce (CApp (AVar f) args) = do
    modify (f :)
    return $ CApp (AVar f) args
betaReduce (CLet def body) = do
    body' <- betaReduce body
    return $ undefined

-- betaReduce (CBinop OP Atom Atom Name CExpr)
-}










type CpsReduce a = State (M.Map Name Int) a


type OptState = (M.Map Name CExpr)


reduce :: CExpr -> Maybe CExpr
reduce (CBinop IAdd (AInt x) (AInt y) z k) = Just $
    Tag (SubstAtom z (AInt (x + y))) k
reduce (CBinop ISub (AInt x) (AInt y) z k) = Just $
    Tag (SubstAtom z (AInt (x - y))) k
-- other binops ......
reduce (CUniop INeg (AInt x) y k) = Just $
    Tag (SubstAtom y (AInt (- x))) k
reduce (CUniop BNot (ABool x) y k) = Just $
    Tag (SubstAtom y (ABool (not x))) k
-- other uniops ......
reduce _ = Nothing

-- reduce (CSelect i (CRecord ([Atom]) Name CExpr)  CExpr)

{-
-- constant reduction
reduce (CLet (x,["k"], CApp (AVar "k") [y]) k) =
    SubstTag x y (reduce k)

-- beta contraction


-- eta reduction
-}


{-


aSubst :: Name -> Atom -> Atom -> Atom
aSubst x a (AVar y)
    | x == y = a
    | otherwise = AVar y
aSubst _ _ other = other


substAtom :: [(Name,Atom)] -> CExpr -> CExpr
substAtom lst = cata go
    where
        subst :: Atom -> Atom
        subst (AVar name) = fromMaybe (AVar name) (lookup name lst)
        subst (ALabel name) = fromMaybe (ALabel name) (lookup name lst)
        subst other = other

        go :: CExprF CExpr -> CExpr
        go (CAppF func args) = CApp (subst func) (fmap subst args)
        go (CBinopF op x y z k) = CBinop op (subst x) (subst y) z k
        go (CUniopF op x y k) = CUniop op (subst x) y k
        -- go (CLetF def expr) = 
        go other = embed other



inlineFunc :: Def -> CExpr -> CExpr
inlineFunc (func,args,body) = cata go
    where
        go :: CExprF CExpr -> CExpr
        go (CAppF (AVar func') args')
            | func' == func =
                let lst = zip args args' in
                undefined

            | otherwise = undefined


type BetaRdc a = State [Name] a

newName :: Name -> BetaRdc ()
newName name = modify (name :)

fetchName :: Name -> BetaRdc Int
fetchName name = do
    xs <- get
    let (n,xs') = go name 0 xs
    put xs'
    return n
    where
        go :: Name -> Int -> [Name] -> (Int,[Name])
        go name n (x:xs)
            | x == name = go name (n + 1) xs
            | otherwise = second (x :) (go name n xs)
        go name n [] = (n,[])


betaScan :: CExpr -> CExpr
betaScan = fst . cata go
    where
        go :: CExprF (CExpr,MS.MultiSet Name) -> (CExpr,MS.MultiSet Name)
        go e@(CAppF (AVar func) args) =
            (embed $ fmap fst e, MS.singleton func)
        go e@(CLetF (func,args,(body,res1)) (expr,res2)) =
            (Tag (BindTimes (MS.occur func res2)) (embed $ fmap fst e)
            , MS.deleteAll func $ res1 `MS.union` res2 )
        go other = (embed $ fmap fst other, MS.empty)






betaReduce :: CExpr -> CExpr
betaReduce = undefined


exprSize :: CExpr -> Int
exprSize = cata go
    where
        go :: CExprF Int -> Int
        go (CLetF _ body) = 1 + body
        go (CAppF _ body) = 1 + undefined
        go (CFixF defs body) = undefined





type GenVar a = State Integer a

cpsTrans :: LExpr ->  (Atom -> GenVar CExpr) -> GenVar CExpr
cpsTrans (LVar x) k = k (AVar x)
cpsTrans (LInt x) k = k (AInt x)
cpsTrans (LReal x) k = k (AReal x)
cpsTrans (LOpr op [x]) k = do
    new <- genvar
    body <- k (AVar new)
    cpsTrans x (\x' ->
        return $ Uniop op x' new body)
cpsTrans (LOpr op [x,y]) k = do
    new <- genvar
    body <- k (AVar new)
    cpsTrans x (\x' -> 
        cpsTrans y (\y' -> do
            return $ Binop op x' y' new body))
cpsTrans (LOpr op _) k = error "?"
cpsTrans (LLam xs body) k =
    f <- genvar
    c <- genvar
    body' <- cpsTrans (\z -> (CApp (AVar c) [z]) body 
    return $ CLet (f,c:args,body') (k (AVar new))
cpsTrans (LApp func args) k = do
    new <- genvar
    body <- k (AVar new)
transArgs :: [LExpr] -> GenVar [CExpr]
transArgs [] = 

-}
