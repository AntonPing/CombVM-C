module Backend.Trans where

import Backend.Core
import Control.Monad.Cont
import Control.Monad.State
import qualified Utils.Symbol as Sym
import Utils.Syntax
import Utils.TopLevel (TopLevel, MonadTopLevel)
import Utils.Symbol

{-
newtype CpsTrans a = CpsTrans
    { unCpsTrans :: ContT CExpr TopLevel a }
    deriving 
        ( Functor, Applicative, Monad
        , MonadCont
        , MonadTopLevel
        , MonadIO
        )
-}

type CpsTrans = ContT CExpr TopLevel


-- changeCont :: (CExpr -> CExpr) -> CpsTrans a -> CpsTrans a


runCpsTrans :: CpsTrans a -> (a -> TopLevel CExpr) -> TopLevel CExpr
runCpsTrans m = runContT m

cpsTransTop :: LExpr -> TopLevel CExpr
cpsTransTop expr = undefined

    {- evalState
    (runContT (cpsTrans expr) (return . CHalt)) 0 -}

cpsTrans :: LExpr -> CpsTrans Atom
cpsTrans (LInt x) = return (AInt x)
cpsTrans (LReal x) = return (AReal x)
cpsTrans (LBool x) = return (ABool x)
cpsTrans (LChar x) = return (AChar x)
cpsTrans (LVar x) = return (AVar x)
cpsTrans (LLam arg body) = do
    body' <- lift $ cpsTransTop body
    f <- genvar 'f'
    ContT $ \k -> do
        res <- k (AVar f)
        return $ CLet [ Def f [arg] body'] res

cpsTrans (LApp func arg) = do
    func' <- cpsTrans func
    arg' <- cpsTrans arg
    r <- genvar 'r'
    ContT $ \k -> do
        res <- k (AVar r)
        return $ CLet [ Def r [] (CApp func' [arg'])] res

cpsTrans (LOpr op args) = do
    args' <- mapM cpsTrans args
    r <- genvar 'r'
    ContT $ \k -> do
        res <- k (AVar r)
        return $ COpr op args' [r] res

cpsTrans (LBrs op args brs) = do
    args' <- mapM cpsTrans args
    brs' <- mapM (lift . cpsTransTop) brs
    return $ CBrs op args' brs'





{-

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
cpsTrans (LRec xs) = do
    xs' <- mapM cpsTrans xs
    new <- genvar
    ContT $ \k -> do
        res <- k (AVar new)
        return $ CRecord (map (PathOff 0) xs') new res
cpsTrans (LGet i x) = do
    x' <- cpsTrans x
    new <- genvar
    ContT $ \k -> do
        res <- k (AVar new)
        return $ CSelect i x' new res



-}