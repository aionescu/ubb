{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tli (
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
version = Version [0,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/oldpug/.cabal/bin"
libdir     = "/home/oldpug/.cabal/lib/x86_64-linux-ghc-8.8.4/tli-0.0.0.0-inplace-tli"
dynlibdir  = "/home/oldpug/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/oldpug/.cabal/share/x86_64-linux-ghc-8.8.4/tli-0.0.0.0"
libexecdir = "/home/oldpug/.cabal/libexec/x86_64-linux-ghc-8.8.4/tli-0.0.0.0"
sysconfdir = "/home/oldpug/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tli_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tli_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tli_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tli_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tli_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tli_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
