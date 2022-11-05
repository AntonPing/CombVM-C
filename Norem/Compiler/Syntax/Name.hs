module Utils.Symbol where
import Prelude
import Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity (Identity (Identity), IdentityT (IdentityT))
import Control.Monad.Writer
import Utils.Intern

data Name
    = Var InternStr
    | Gen Char Int
    deriving (Eq, Ord, Show)

genvar :: (MonadTopLevel m, MonadIO m) => Char -> m Name
genvar ch = Gen ch <$> fresh

data Config = Config
    { filename :: String
    , machineTarget :: String
    , regNumber :: Int
    }
