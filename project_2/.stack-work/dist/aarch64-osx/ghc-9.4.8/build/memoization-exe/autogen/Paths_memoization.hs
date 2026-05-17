{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_memoization (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/simongustafsson/Desktop/Bachelors in Mathematics/EDAN40/project_2/.stack-work/install/aarch64-osx/a47b28e08280c1d53b2405b76b6efedba2ec2fa22ccade886cc50b89201311fd/9.4.8/bin"
libdir     = "/Users/simongustafsson/Desktop/Bachelors in Mathematics/EDAN40/project_2/.stack-work/install/aarch64-osx/a47b28e08280c1d53b2405b76b6efedba2ec2fa22ccade886cc50b89201311fd/9.4.8/lib/aarch64-osx-ghc-9.4.8/memoization-0.1.0.0-BtKw2Bo1SCkHxxWIM4bDjc-memoization-exe"
dynlibdir  = "/Users/simongustafsson/Desktop/Bachelors in Mathematics/EDAN40/project_2/.stack-work/install/aarch64-osx/a47b28e08280c1d53b2405b76b6efedba2ec2fa22ccade886cc50b89201311fd/9.4.8/lib/aarch64-osx-ghc-9.4.8"
datadir    = "/Users/simongustafsson/Desktop/Bachelors in Mathematics/EDAN40/project_2/.stack-work/install/aarch64-osx/a47b28e08280c1d53b2405b76b6efedba2ec2fa22ccade886cc50b89201311fd/9.4.8/share/aarch64-osx-ghc-9.4.8/memoization-0.1.0.0"
libexecdir = "/Users/simongustafsson/Desktop/Bachelors in Mathematics/EDAN40/project_2/.stack-work/install/aarch64-osx/a47b28e08280c1d53b2405b76b6efedba2ec2fa22ccade886cc50b89201311fd/9.4.8/libexec/aarch64-osx-ghc-9.4.8/memoization-0.1.0.0"
sysconfdir = "/Users/simongustafsson/Desktop/Bachelors in Mathematics/EDAN40/project_2/.stack-work/install/aarch64-osx/a47b28e08280c1d53b2405b76b6efedba2ec2fa22ccade886cc50b89201311fd/9.4.8/etc"

getBinDir     = catchIO (getEnv "memoization_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "memoization_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "memoization_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "memoization_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "memoization_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "memoization_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
