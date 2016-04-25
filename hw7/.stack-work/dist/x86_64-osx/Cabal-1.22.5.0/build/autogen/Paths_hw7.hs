module Paths_hw7 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/deathgrindfreak/bin/cis194/hw7/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/bin"
libdir     = "/Users/deathgrindfreak/bin/cis194/hw7/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/lib/x86_64-osx-ghc-7.10.3/hw7-0.1.0.0-3QkzTrwDgZRB64IaP3152p"
datadir    = "/Users/deathgrindfreak/bin/cis194/hw7/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/share/x86_64-osx-ghc-7.10.3/hw7-0.1.0.0"
libexecdir = "/Users/deathgrindfreak/bin/cis194/hw7/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/libexec"
sysconfdir = "/Users/deathgrindfreak/bin/cis194/hw7/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw7_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw7_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hw7_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw7_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw7_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
