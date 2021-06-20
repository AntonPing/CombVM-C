module Norem (module Norem) where
import Prelude
import Control.Monad
import System.IO
import System.Process
--import Data.Map (Map, lookup)
import Compiler
import Data.Void
import Foreign.Marshal
import Foreign.Ptr

newtype Runtime = Runtime {
    stack :: [Obj],
    dict  :: [(String,Obj)]
}

newtype Op a = Op (a -> IO a)

runOp :: Opration -> Runtime -> IO Runtime
runOp (Opration f) rt = f rt 

mkPureOp :: (Runtime -> Runtime) -> Opration
mkPureOp f = Opration (\rt -> return (f rt))

instance Functor Opration where
    -- fmap :: (a -> b) -> t a -> t b
    fmap f op = Opration (\rt




    )

instance Applicative Opration where
   -- pure :: a -> t a
   pure f = Opration 

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices








newtype Func = Func (Runtime -> IO(Runtime))
instance Show Func where
    show fn = "?"


runFunc :: Func -> Runtime -> IO(Runtime)
runFunc (Func fn) x = fn x

data Obj =
      OApp Obj Obj
    | OLink String
    | OFunc Func
    | OInt Int
    | OReal Double
    | OPtr (Ptr Void)
    deriving (Show)
    

type Stack = [Obj]
type Dict = [(String,Obj)]
type Runtime = (Stack,Dict)

runStep :: Runtime -> IO(Runtime)
runStep (OApp t1 t2:xs, dict) = 
    runStep (t1:t2:xs, dict)
runStep a@(OFunc fn:xs, dict) = 
    runFunc fn a
runStep (OLink key:xs,dict) = 
    let value = lookup key dict in
    case value of
        Just obj -> return (obj:xs,dict)
        Nothing -> return ([],dict) 
runStep other = return other

evalLoop :: Runtime -> IO(Runtime)
evalLoop rt = do
    print rt
    rt' <- runStep rt
    return ([],[])




