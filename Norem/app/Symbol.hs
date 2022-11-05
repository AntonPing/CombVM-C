{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Symbol (
    Name, nameVar, genVar
) where

import Control.Applicative
import Control.Monad

import Control.Effect.Labelled
import Control.Effect.State
import Control.Carrier.State.Strict

import Control.Lens

import Data.Kind
import Data.Map as M
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Control.Carrier.Lift (runM)

data Name =
      NameVar (String,Int) Int
    | GenVar Char Int

instance Eq Name where
    NameVar (_,i1) n1 == NameVar (_,i2) n2 =
        i1 == i2 && n1 == n2
    GenVar i1 n1 == GenVar i2 n2 =
        i1 == i2 && n1 == n2
    _ == _ = False

instance Show Name where
    show (NameVar (str,_) n) = str ++ show n
    show (GenVar ch n) = show ch ++ show n

data GenSym (m :: Type -> Type) k where
    OpNameVar :: String -> GenSym m Name
    OpGenVar :: Char -> GenSym m Name

nameVar :: Has GenSym sig m => String -> m Name
nameVar str = send (OpNameVar str)

genVar :: Has GenSym sig m => Char -> m Name
genVar ch = send (OpGenVar ch)

data SymTable = SymTable
    { -- bijection from Int to Text
      _strCount :: Int
    , _strIdMap :: M.Map String Int
    , _idStrMap :: M.Map Int String
    -- map for name, use Int Id as key
    , _nameMap :: M.Map Int Int
    -- map for gen
    , _genMap :: M.Map Char Int
    }

makeLenses ''SymTable

newtype GenSymC m a = GenSymC { runGenSymC :: StateC SymTable m a }
    deriving (Functor, Applicative, Monad)

instance (Monad m, Algebra sig m) =>
    Algebra (GenSym :+: sig) (GenSymC m) where
    alg hdl sig ctx = GenSymC $ case sig of
        L (OpNameVar str) -> (<$ ctx) <$> do
            table <- get @SymTable
            case M.lookup str (view strIdMap table) of
                Just i -> undefined
                Nothing -> undefined
        L (OpGenVar ch) -> (<$ ctx) <$> do
            table <- get @SymTable
            let i = fromMaybe 0 $ M.lookup ch $ view genMap table
            modify $ over genMap $ M.insert ch (i+1)
            return $ GenVar ch i

        R other -> alg (runGenSymC . hdl) (R other) ctx
    {-# INLINE alg #-}

defaultSymTable :: SymTable
defaultSymTable = SymTable
    { _strCount = 0
    , _strIdMap = M.empty
    , _idStrMap = M.empty
    , _nameMap = M.empty
    , _genMap = M.empty
    }

runGenSym :: Functor m => GenSymC m a -> m a
runGenSym = evalState defaultSymTable . runGenSymC


doIt :: IO ()
doIt = (runM . evalState ("foo"::String) . runGenSym) genPrint

genPrint :: (Has GenSym sig m, Has (State String) sig m)
    => m ()
genPrint = do
    str <- get @String
    x <- nameVar str
    put (show x)
    return ()

