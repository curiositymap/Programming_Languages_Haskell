{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Practice1 (
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

bindir     = "/Users/destiny/.cabal/bin"
libdir     = "/Users/destiny/.cabal/lib/x86_64-osx-ghc-8.10.1/Practice1-0.1.0.0-inplace-Practice1"
dynlibdir  = "/Users/destiny/.cabal/lib/x86_64-osx-ghc-8.10.1"
datadir    = "/Users/destiny/.cabal/share/x86_64-osx-ghc-8.10.1/Practice1-0.1.0.0"
libexecdir = "/Users/destiny/.cabal/libexec/x86_64-osx-ghc-8.10.1/Practice1-0.1.0.0"
sysconfdir = "/Users/destiny/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Practice1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Practice1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Practice1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Practice1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Practice1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Practice1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
