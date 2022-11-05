{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE LambdaCase #-}

module Trans where
import Utils
import Data.Maybe
import Data.Functor.Foldable
import qualified Data.MultiSet as MS
import qualified Data.Set as S

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity (Identity(runIdentity))
import qualified Data.Text as T



embedM :: Monad m => CExprF (m CExpr) -> m CExpr
embedM (CAppF func args) =
    return $ CApp func args
embedM (CLetF (func,args,body) expr) = do
    body' <- body
    expr' <- expr
    return $ CLet (func,args,body') expr'
embedM (CFixF defs expr) = do
    defs' <- mapM bindDef defs
    expr' <- expr
    return $ CFix defs' expr'
    where
        bindDef :: Monad m => (Name,[Name],m CExpr) -> m (Name,[Name],CExpr)
        bindDef (func,args,body) = do
            body' <- body
            return (func,args,body')
embedM (CBinopF op x y z k) =
    CBinop op x y z <$> k
embedM (CUniopF op x y k) =
    CUniop op x y <$> k
embedM (CRecordF paths x k) =
    CRecord paths x <$> k
embedM (CSelectF n r x k) =
    CSelect n r x <$> k
embedM (CIfteF p trbr flbr) = do
    trbr' <- trbr
    flbr' <- flbr
    return $ CIfte p trbr' flbr'
embedM (CSwitchF n brs) = do
    brs' <- sequence brs
    return $ CSwitch n brs'
embedM (CHaltF x) =
    return $ CHalt x
embedM (TagF tag expr) =
    Tag tag <$> expr

-- type Pass = State Int
-- type CountT m = StateT Int m

{-
aSubst :: [(Name,Atom)] -> Atom -> Atom
aSubst env a@(AVar name) = fromMaybe a $ lookup name env
aSubst env a@(ALabel name) = fromMaybe a $ lookup name env
aSubst _ other = other

pSubst :: [(Name,Atom)] -> AccessPath -> AccessPath
pSubst env (PathOff n a) = PathOff n (aSubst env a)
pSubst env (PathSel n p) = PathSel n (pSubst env p)
-}

type Pass = StateT (Int,Int) IO

runPass :: Pass a -> (Int,Int) -> IO a
runPass = evalStateT

newvar :: Pass Name
newvar = do
    (n,m) <- get
    put (n,m + 1)
    return $ T.pack ('#': show m)

count :: Pass ()
count = do
    (n,m) <- get
    put (n + 1,m)

getCount :: Pass Int
getCount = gets fst

emptyCount :: Pass ()
emptyCount = do
    (_,m) <- get
    put (0,m)

type AtomSubst a = ReaderT [(Name,Atom)] Pass a

aSubst :: Atom -> AtomSubst Atom
aSubst (AVar name) = do
    val <- asks $ lookup name
    case val of
        Just a -> lift count >> return a
        Nothing -> return (AVar name)
aSubst (ALabel name) = do
    val <- asks $ lookup name
    case val of
        Just a -> lift count >> return a
        Nothing -> return (ALabel name)
aSubst other = return other

pSubst :: AccessPath -> AtomSubst AccessPath
pSubst (PathOff n a) = do
    a' <- aSubst a
    return $ PathOff n a'
pSubst (PathSel n p) = do
    p' <- pSubst p
    return $ PathSel n p'

atomSubst :: CExpr -> Pass CExpr
atomSubst expr = runReaderT (cata go expr) []
    where
    go :: CExprF (AtomSubst CExpr) -> AtomSubst CExpr
    go (CAppF func args) = do
        func' <- aSubst func
        args' <- mapM aSubst args
        return $ CApp func' args'
    go (CLetF (func,args,body) expr) = do
        body' <- body
        expr' <- expr
        return $ CLet (func,args, body') expr'
    go (CFixF defs expr) = do
        defs' <- forM defs (\(func,args,body) -> do
            body' <- body
            return (func, args, body')
            )
        expr' <- expr
        return $ CFix defs' expr'
    go (CBinopF op x y r k) = do
        x' <- aSubst x
        y' <- aSubst y
        k' <- k
        return $ CBinop op x' y' r k'
    go (CUniopF op x r k) = do
        x' <- aSubst x
        k' <- k
        return $ CUniop op x' r k'
    go (CSelectF n x r k) = do
        x' <- aSubst x
        k' <- k
        return $ CSelect n x' r k'
    go (CRecordF paths r k) = do
        paths' <- mapM pSubst paths
        k' <- k
        return $ CRecord paths' r k'
    go (CIfteF p trbr flbr) = do
        p' <- aSubst p
        trbr' <- trbr
        flbr' <- flbr
        return $ CIfte p' trbr' flbr'
    go (CSwitchF x brs) = do
        x' <- aSubst x
        brs' <- sequence brs
        return $ CSwitch x' brs'
    go (TagF (SubstAtom name atom) expr) =
        local ((name,atom) :) expr
    go (TagF tag expr) = expr
    go (CHaltF x) = CHalt <$> aSubst x


type DeadElim a = StateT (MS.MultiSet Name) Pass a

deadElem :: CExpr -> Pass CExpr
deadElem expr = evalStateT (cata go expr) MS.empty
    where
    go :: CExprF (DeadElim CExpr) -> DeadElim CExpr
    go (CAppF func args) = do
        forM_ (func:args) $ \case
            AVar f -> modify $ MS.insert f
            ALabel f -> modify $ MS.insert f
            _ -> pure ()
        return $ CApp func args
    go (CLetF (func,args,body) expr) = do
        expr' <- expr
        body' <- body
        n <- gets $ MS.occur func
        modify $ MS.deleteAll func
        if n == 0 then do
            lift count
            return expr'
        else return $ CLet (func,args,body') expr'
    go other = embedM other

type BetaScan a = StateT (MS.MultiSet Name) Pass a

betaScan :: CExpr -> Pass CExpr
betaScan expr = evalStateT (cata go expr) MS.empty
    where
    go :: CExprF (BetaScan CExpr) -> BetaScan CExpr
    go (CAppF func args) = do
        case func of
            AVar f -> modify (MS.insert f)
            ALabel f -> modify (MS.insert f)
            _ -> pure ()
        return $ CApp func args
    go (CLetF (func,args,body) expr) = do
        expr' <- expr
        body' <- body
        n <- gets $ MS.occur func
        modify $ MS.deleteAll func
        if n == 1
        then do
            lift count
            return $ Tag (SubstFunc (func,args,body'))
                (CLet (func,args,body') expr')
        else return $ CLet (func,args,body') expr'
    go other = embedM other


type BetaReduce a = ReaderT [Def] Pass a

funcMatch :: [Def] -> CExpr -> CExpr
funcMatch [] expr = expr
funcMatch ((func,args,body):rest) e@(CApp (AVar func') args')
    | func == func' = let binds = zip args args' in
        foldr (\(x,y) acc -> Tag (SubstAtom x y) acc) body binds
funcMatch ((func,args,body):rest) expr =
    funcMatch rest expr

betaReduce :: CExpr -> Pass CExpr
betaReduce expr = runReaderT (cata go expr) []
    where
    go :: CExprF (BetaReduce CExpr) -> BetaReduce CExpr
    go (CAppF func args) = do
        env <- ask
        return $ funcMatch env (CApp func args)
    go (TagF (SubstFunc def) expr) = do
        local (def :) expr
    go other = embedM other


constFold :: CExpr -> Pass CExpr
constFold = cata go
    where
    go :: CExprF (Pass CExpr) -> Pass CExpr
    go (CBinopF IAdd (AInt x) (AInt y) z k) =
        count >> k >>= \k' -> return $
            Tag (SubstAtom z (AInt (x + y))) k'
    go (CBinopF ISub (AInt x) (AInt y) z k) =
        count >> k >>= \k' -> return $
            Tag (SubstAtom z (AInt (x - y))) k'
    go (CBinopF IMul (AInt x) (AInt y) z k) =
        count >> k >>= \k' -> return $
            Tag (SubstAtom z (AInt (x * y))) k'
    go (CBinopF IDiv (AInt x) (AInt y) z k) =
        count >> k >>= \k' -> return $
            Tag (SubstAtom z (AInt (x `div` y))) k'
    -- other binops ......
    go (CUniopF INeg (AInt x) y k) =
        count >> k >>= \k' -> return $
            Tag (SubstAtom y (AInt (- x))) k'
    go (CUniopF BNot (ABool x) y k) =
        count >> k >>= \k' -> return $
            Tag (SubstAtom y (ABool (not x))) k'
    -- other uniops ......
    go other = embedM other


type ClosScan a = StateT (S.Set Name) Pass a

closScan :: CExpr -> Pass CExpr
closScan expr = evalStateT (cata go expr) S.empty
    where
    go :: CExprF (ClosScan CExpr) -> ClosScan CExpr
    go (CAppF func args) = do
        forM_ (func:args) $ \case
            AVar f -> modify $ S.insert f
            ALabel f -> modify $ S.insert f
            _ -> pure ()
        func' <- lift newvar
        return $ CSelect 0 func func'$
            CApp (AVar func') (func:args)
    go (CLetF (func,args,body) expr) = do
        body' <- body
        forM_ args $ \arg ->
            modify (S.delete arg)
        vars <- get
        put S.empty
        expr' <- expr

        clos <- lift newvar
        func' <- lift newvar
        -- with tag in body'
        let pair = zip [1..] (S.elems vars)
        let f (n,arg) rest = CSelect n (AVar clos) arg rest 
        let newbody = foldr f body' pair

        return $ CLet (func', clos:args, newbody) $
            CRecord (map (PathOff 0 . AVar) (func':S.elems vars)) func expr'

    go other = embedM other


{-

type ClosConv a = ReaderT (S.Set Name) Pass a

closConv :: CExpr -> Pass CExpr
closConv expr = runReaderT (cata go expr) S.empty
    where
    go :: CExprF (ClosConv CExpr) -> ClosConv CExpr
    go (CAppF (AVar func) args) = do
        p <- asks $ S.member func
        x <- lift newvar
        return $ if p
        then CSelect 0 (AVar func) x (CApp (AVar x) args)
        else CApp (AVar func) args

    go (TagF (FreeVar vars) (LetF (func,args,body) expr)) = do
        body' <- body
        expr' <- expr
        
        clos <- lift newvar
        return $ Let (func,clos:args,body) expr)
    go other = embedM other

-}