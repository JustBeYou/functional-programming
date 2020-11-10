{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_compiler (
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

bindir     = "/home/littlewho/.cabal/bin"
libdir     = "/home/littlewho/.cabal/lib/x86_64-linux-ghc-8.6.5/compiler-0.1.0.0-51pjGIsSLvn36PMV4aJTX2-compiler"
dynlibdir  = "/home/littlewho/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/littlewho/.cabal/share/x86_64-linux-ghc-8.6.5/compiler-0.1.0.0"
libexecdir = "/home/littlewho/.cabal/libexec/x86_64-linux-ghc-8.6.5/compiler-0.1.0.0"
sysconfdir = "/home/littlewho/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "compiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "compiler_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "compiler_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "compiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "compiler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "compiler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
