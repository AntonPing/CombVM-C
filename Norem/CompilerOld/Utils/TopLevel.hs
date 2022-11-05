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

data TopEnv = TopEnv
    { filename :: String
    , verbosity :: Int
    , freshVar :: IORef Int
    , internMapRef :: IORef InternMap
    }

newtype TopLevel a = TopLevel
    { unTopLevel :: ReaderT TopEnv IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

runTopLevel :: TopLevel a -> TopEnv -> IO a
runTopLevel (TopLevel m) = runReaderT m

runTopLevelDefault :: TopLevel a -> IO a
runTopLevelDefault m = runTopLevel m $ TopEnv
    { filename = "unknown"
    , verbosity = 1
    , freshVar = unsafePerformIO $ newIORef 0
    , internMapRef = unsafePerformIO $ newIORef emptyInternMap
    }

class Monad m => MonadTopLevel m where
    askEnv :: m TopEnv
    localEnv :: (TopEnv -> TopEnv) -> m a -> m a

asksEnv :: MonadTopLevel m => (TopEnv -> a) -> m a
asksEnv f = f <$> askEnv

instance MonadTopLevel TopLevel where
    askEnv =  TopLevel ask
    localEnv f (TopLevel m) = TopLevel $ local f m

instance MonadTopLevel m => MonadTopLevel (ReaderT r m) where
    askEnv = lift askEnv
    localEnv = mapReaderT . localEnv

instance (MonadTopLevel m, Monoid w) => MonadTopLevel (WriterT w m) where
    askEnv = lift askEnv
    localEnv = mapWriterT . localEnv

instance MonadTopLevel m => MonadTopLevel (StateT s m) where
    askEnv = lift askEnv
    localEnv = mapStateT . localEnv

instance MonadTopLevel m => MonadTopLevel (ExceptT e m) where
    askEnv = lift askEnv
    localEnv = mapExceptT . localEnv

instance MonadTopLevel m => MonadTopLevel (ContT e m) where
    askEnv = lift askEnv
    localEnv = mapContT . localEnv

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