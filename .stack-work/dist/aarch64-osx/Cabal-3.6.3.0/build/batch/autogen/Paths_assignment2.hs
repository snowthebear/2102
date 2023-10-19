{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_assignment2 (
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
bindir     = "/Users/Monash Uni/Year 2 sem 1/FIT2102/A2/.stack-work/install/aarch64-osx/d859c3e12a5045183e1d4c98309c3bfefbb90dab976d8fd96657fefdec7f6626/9.2.8/bin"
libdir     = "/Users/Monash Uni/Year 2 sem 1/FIT2102/A2/.stack-work/install/aarch64-osx/d859c3e12a5045183e1d4c98309c3bfefbb90dab976d8fd96657fefdec7f6626/9.2.8/lib/aarch64-osx-ghc-9.2.8/assignment2-0.1.0.0-CfHV6U9x8x2FrZ5uJGU9F7-batch"
dynlibdir  = "/Users/Monash Uni/Year 2 sem 1/FIT2102/A2/.stack-work/install/aarch64-osx/d859c3e12a5045183e1d4c98309c3bfefbb90dab976d8fd96657fefdec7f6626/9.2.8/lib/aarch64-osx-ghc-9.2.8"
datadir    = "/Users/Monash Uni/Year 2 sem 1/FIT2102/A2/.stack-work/install/aarch64-osx/d859c3e12a5045183e1d4c98309c3bfefbb90dab976d8fd96657fefdec7f6626/9.2.8/share/aarch64-osx-ghc-9.2.8/assignment2-0.1.0.0"
libexecdir = "/Users/Monash Uni/Year 2 sem 1/FIT2102/A2/.stack-work/install/aarch64-osx/d859c3e12a5045183e1d4c98309c3bfefbb90dab976d8fd96657fefdec7f6626/9.2.8/libexec/aarch64-osx-ghc-9.2.8/assignment2-0.1.0.0"
sysconfdir = "/Users/Monash Uni/Year 2 sem 1/FIT2102/A2/.stack-work/install/aarch64-osx/d859c3e12a5045183e1d4c98309c3bfefbb90dab976d8fd96657fefdec7f6626/9.2.8/etc"

getBinDir     = catchIO (getEnv "assignment2_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "assignment2_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "assignment2_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "assignment2_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "assignment2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "assignment2_sysconfdir") (\_ -> return sysconfdir)




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
