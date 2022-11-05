module Utils.TopLevel where
import Prelude
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State as Strict
import Control.Monad.Writer
import Data.Map as M
import Data.IORef
import Data.Maybe (fromJust)
import Control.Monad.Except
import GHC.IO (unsafePerformIO)
import Control.Monad.Cont


data InternMap = InternMap
    { strToInt :: M.Map String Integer
    , intToStr :: M.Map Integer String
    , counter :: Integer
    }

emptyInternMap :: InternMap
emptyInternMap = InternMap
    { strToInt = M.empty
    , intToStr = M.empty
    , counter = 0
    }

newtype InternStr = InternStr Integer
    deriving (Eq, Show, Ord)

intern :: (MonadTopLevel m, MonadIO m) => String -> m InternStr
intern s = do
    ref <- asksEnv internMapRef
    InternMap { .. } <- liftIO $ readIORef ref
    case M.lookup s strToInt of
        Just i -> return $ InternStr i
        Nothing -> do
            liftIO $ writeIORef ref $ InternMap
                { strToInt = M.insert s counter strToInt
                , intToStr = M.insert counter s intToStr
                , counter = counter + 1
                }
            return $ InternStr counter

toStr :: (MonadTopLevel m, MonadIO m) => InternStr -> m String
toStr (InternStr i) = do
    ref <- asksEnv internMapRef
    InternMap { .. } <- liftIO $ readIORef ref
    return $ fromJust $ M.lookup i intToStr


fresh :: (MonadTopLevel m, MonadIO m) => m Int
fresh = do
    ref <- asksEnv freshVar
    n <- liftIO $ readIORef ref
    liftIO $ writeIORef ref (n+1)
    return n