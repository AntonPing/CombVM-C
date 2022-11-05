{-# LANGUAGE OverloadedStrings #-}

module RegAlloc where

import Utils
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Identity (Identity)
import Data.Functor.Identity

import qualified Data.Map as M
import qualified Data.Text as T
import Debug.Trace (trace)

import Pretty


type RegAlloc a = State [Int] a


alloc :: RegAlloc Int
alloc = do
    regs <- get
    case regs of
        (x:xs) -> do
            put xs
            return x
        [] -> error "register not enough, please do Spilling brefore RegAlloc."


regAlloc :: CExpr -> RegAlloc CExpr
regAlloc (CApp func args) = do
    func




