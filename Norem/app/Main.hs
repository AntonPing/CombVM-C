module Main where

import Parser
import Pretty
import CPS (cpsTransTop)
import Trans
import Utils
import Control.Monad


printExpr :: CExpr -> Pass CExpr
printExpr expr = do
    lift $ print expr
    lift $ putStrLn "---------------------------"
    return expr

leastCount :: Int -> (CExpr -> Pass CExpr) -> CExpr -> Pass CExpr
leastCount n ps expr = do
    expr' <- ps expr
    m <- getCount
    if m > n then do
        emptyCount
        leastCount n ps expr'
    else return expr'

times :: Int -> (CExpr -> Pass CExpr) -> CExpr -> Pass CExpr
times 0 ps expr = return expr 
times n ps expr = do
    expr' <- ps expr
    times (n - 1) ps expr'

passList :: [CExpr -> Pass CExpr] -> CExpr -> Pass CExpr
passList = foldl1 (>=>)

testPass :: CExpr -> Pass CExpr
testPass = 
    times 1 $ passList
        
        [
        {- times 1 $ passList
            [ deadElem
            , printExpr
            ]
        , times 1 $ passList 
            [ betaScan
            , betaReduce
            , atomSubst
            , printExpr
            ]
        , times 1 $ passList 
            [ constFold
            , atomSubst
            , printExpr
            ]
        -}
          passList
            [ closScan
            , printExpr
            ]
        , \expr -> lift $ putStrLn "again!" >> return expr
        ]
    


{-
runPass :: [Pass] -> CExpr -> CountT IO CExpr
runPass pss expr = foldM f expr pss
    where
    f :: CExpr -> Pass -> CountT IO CExpr
    f expr ps = do
        lift $ print expr
        expr' <- ps expr
        lift $ putStrLn "----------------"
        return expr'
-}

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    case parseExpr test3 of
        Left e -> do
            putStrLn "parse failed!"
            print e
        Right e -> do
            putStrLn "parse success!"
            let expr = cpsTransTop e
            putStrLn "Lambda Expression:"
            print expr
            putStrLn "----------------"
            expr3 <- runPass (testPass expr) (0,0)
            print expr3


test :: String
test = "((fn x y => (mul (add x 1) (sub y 2))) 3 4)"

test2 :: String
test2 = "fn x y => [{x:2},y,3]"

test3 :: String
test3 = "fn x => fn y => add x y"