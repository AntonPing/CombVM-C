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

