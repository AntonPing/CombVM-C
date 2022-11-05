module Test.Test where
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State as Strict
import Control.Monad.Writer
import Data.Map as M
import Data.IORef
import Data.Maybe (fromJust)
import Control.Monad.Except
import GHC.IO (unsafePerformIO)
import Utils.TopLevel
import Frontend.Lexer
import Frontend.Token


lexerTest :: String -> IO ()
lexerTest str = runTopLevelDefault $ do
    res <- evalLexerStr tokenize str
    liftIO $ print res
    liftIO $ putStrLn "hello world"
