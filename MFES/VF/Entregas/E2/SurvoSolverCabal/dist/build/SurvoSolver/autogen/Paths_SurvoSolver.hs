{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_SurvoSolver (
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

bindir     = "/home/pcosta/.cabal/bin"
libdir     = "/home/pcosta/.cabal/lib/x86_64-linux-ghc-8.6.5/SurvoSolver-0.1.0.0-BXszMszkc3l3a5E3tReNO0-SurvoSolver"
dynlibdir  = "/home/pcosta/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/pcosta/.cabal/share/x86_64-linux-ghc-8.6.5/SurvoSolver-0.1.0.0"
libexecdir = "/home/pcosta/.cabal/libexec/x86_64-linux-ghc-8.6.5/SurvoSolver-0.1.0.0"
sysconfdir = "/home/pcosta/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SurvoSolver_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SurvoSolver_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SurvoSolver_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SurvoSolver_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SurvoSolver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SurvoSolver_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
