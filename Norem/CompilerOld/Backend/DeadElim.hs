module Backend.DeadElim where

import Backend.Core
import Control.Monad.Cont
import Control.Monad.State
import qualified Utils.Symbol as Sym
import Utils.TopLevel (TopLevel, MonadTopLevel)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe (fromJust, fromMaybe)
import Utils.Syntax
import Utils.Symbol

type DeadElim = WriterT (S.Set Name) TopLevel

-- reachable :: M.Map Name [Name] -> [Name]

visitAtom :: Atom -> DeadElim Atom
visitAtom (AVar f) = do
    tell $ S.singleton f
    return $ AVar f
visitAtom atom = return atom

visitDef :: Def -> DeadElim Def
visitDef (Def name args body) = do
    undefined

visitExpr :: CExpr -> DeadElim CExpr
visitExpr (CApp func args) = do
    func' <- visitAtom func
    args' <- mapM visitAtom args
    return $ CApp func' args'
visitExpr (CLet [] body) =
    return body
visitExpr (CLet defs body) = do
    (body', fv) <- listen $ visitExpr body
    lst <- mapM (listen . visitDef) defs
    let (defs', fvs) = unzip lst
    let names = S.fromList $ map defFunc defs'
    let root = fv `S.intersection` names

    let refer = flip map lst $ \(def, fv) ->
            (defFunc def, fv `S.intersection` names)

    let chain = flip iterate root $
            S.unions . S.map (fromMaybe S.empty . (`lookup` refer))

    let fixpoint (x:y:res) = if x == y then x else fixpoint (y:res)

    let result = fixpoint chain

    return $ CLet (filter ((`S.member` result) . defFunc) defs') body

visitExpr (COpr op args binds cont) = do
    (cont', fv) <- listen $ visitExpr cont
    pass $ return ((), (`S.difference` S.fromList binds))
    if not (any (`S.member` fv) binds) && isPurePrim op
    then return cont'
    else do
        args' <- mapM visitAtom args
        return $ COpr op args' binds cont'
visitExpr (CBrs op args conts) = do
    conts' <- mapM visitExpr conts
    args' <- mapM visitAtom args
    return $ CBrs op args' conts'
visitExpr (CRec len bind cont) = do
    (cont', tv) <- listen $ visitExpr cont
    pass $ return ((), S.delete bind)
    if not $ bind `S.member` tv
    then return cont'
    else return $ CRec len bind cont
visitExpr (CSet rcd idx arg cont) = do
    undefined
    



visitExpr _ = undefined


{-
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
-}