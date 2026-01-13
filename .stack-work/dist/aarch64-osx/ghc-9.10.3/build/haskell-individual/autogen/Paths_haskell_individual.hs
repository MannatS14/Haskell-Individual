{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haskell_individual (
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
bindir     = "/Users/mannatsingh/Downloads/haskell-individual/.stack-work/install/aarch64-osx/036bdb8f87d50f69d4ef953618be31a7728429705ee0a9e0b724aef145b15a66/9.10.3/bin"
libdir     = "/Users/mannatsingh/Downloads/haskell-individual/.stack-work/install/aarch64-osx/036bdb8f87d50f69d4ef953618be31a7728429705ee0a9e0b724aef145b15a66/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c/haskell-individual-0.1.0.0-BStHjXrwJ6jBomuroPrAL3-haskell-individual"
dynlibdir  = "/Users/mannatsingh/Downloads/haskell-individual/.stack-work/install/aarch64-osx/036bdb8f87d50f69d4ef953618be31a7728429705ee0a9e0b724aef145b15a66/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c"
datadir    = "/Users/mannatsingh/Downloads/haskell-individual/.stack-work/install/aarch64-osx/036bdb8f87d50f69d4ef953618be31a7728429705ee0a9e0b724aef145b15a66/9.10.3/share/aarch64-osx-ghc-9.10.3-fe9c/haskell-individual-0.1.0.0"
libexecdir = "/Users/mannatsingh/Downloads/haskell-individual/.stack-work/install/aarch64-osx/036bdb8f87d50f69d4ef953618be31a7728429705ee0a9e0b724aef145b15a66/9.10.3/libexec/aarch64-osx-ghc-9.10.3-fe9c/haskell-individual-0.1.0.0"
sysconfdir = "/Users/mannatsingh/Downloads/haskell-individual/.stack-work/install/aarch64-osx/036bdb8f87d50f69d4ef953618be31a7728429705ee0a9e0b724aef145b15a66/9.10.3/etc"

getBinDir     = catchIO (getEnv "haskell_individual_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haskell_individual_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haskell_individual_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haskell_individual_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_individual_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_individual_sysconfdir") (\_ -> return sysconfdir)



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
