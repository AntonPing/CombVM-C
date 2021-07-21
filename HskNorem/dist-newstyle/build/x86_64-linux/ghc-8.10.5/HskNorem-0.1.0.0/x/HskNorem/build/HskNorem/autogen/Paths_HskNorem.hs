{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_HskNorem (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anton/.cabal/bin"
libdir     = "/home/anton/.cabal/lib/x86_64-linux-ghc-8.10.5/HskNorem-0.1.0.0-inplace-HskNorem"
dynlibdir  = "/home/anton/.cabal/lib/x86_64-linux-ghc-8.10.5"
datadir    = "/home/anton/.cabal/share/x86_64-linux-ghc-8.10.5/HskNorem-0.1.0.0"
libexecdir = "/home/anton/.cabal/libexec/x86_64-linux-ghc-8.10.5/HskNorem-0.1.0.0"
sysconfdir = "/home/anton/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HskNorem_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HskNorem_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HskNorem_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HskNorem_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HskNorem_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HskNorem_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
