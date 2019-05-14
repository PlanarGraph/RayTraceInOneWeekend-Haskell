{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_raytrace (
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

bindir     = "/Users/nolantiffney/haskell/raytrace/.stack-work/install/x86_64-osx/lts-12.10/8.4.3/bin"
libdir     = "/Users/nolantiffney/haskell/raytrace/.stack-work/install/x86_64-osx/lts-12.10/8.4.3/lib/x86_64-osx-ghc-8.4.3/raytrace-0.1.0.0-DA41VseRCpx3rR4fL5DUdq"
dynlibdir  = "/Users/nolantiffney/haskell/raytrace/.stack-work/install/x86_64-osx/lts-12.10/8.4.3/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/nolantiffney/haskell/raytrace/.stack-work/install/x86_64-osx/lts-12.10/8.4.3/share/x86_64-osx-ghc-8.4.3/raytrace-0.1.0.0"
libexecdir = "/Users/nolantiffney/haskell/raytrace/.stack-work/install/x86_64-osx/lts-12.10/8.4.3/libexec/x86_64-osx-ghc-8.4.3/raytrace-0.1.0.0"
sysconfdir = "/Users/nolantiffney/haskell/raytrace/.stack-work/install/x86_64-osx/lts-12.10/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "raytrace_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "raytrace_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "raytrace_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "raytrace_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "raytrace_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "raytrace_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
