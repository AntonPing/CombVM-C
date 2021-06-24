module Norem (module Norem) where
import Prelude
import Control.Monad
import System.IO
import System.Process
import Data.Void
import NoremParser
import NoremRuntime


eval :: String -> Maybe Term
eval s = fmap run (parse s)


fact = parse "letrec fact (\\x.\
                \ if (== x 0) 1; \
                \ * x (fact (- x 1)))\
            \ (fact 3)"


count = parse "letrec count (\\x. \
                \ if (== x 0) 0 (count (- x 1))) \
            \ (count 0)"

test = parse "(\\x.\\y. > x y) 1 2"

test2 = parse "let x (3) x"

test3 = "\\x. if (== x 0) 1 ;\
             \if (== x 1) 1 ;\
                \+ (fib ; - x 1)\
                  \(fib ; - x 2)"