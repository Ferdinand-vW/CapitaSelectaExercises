module Paths_Exercise42 (
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

bindir     = "/home/ferdinand/.cabal/bin"
libdir     = "/home/ferdinand/.cabal/lib/x86_64-linux-ghc-7.10.3/Exercise42-0.1.0.0-48NNxU8u2cCIKtc2lcDH1p"
datadir    = "/home/ferdinand/.cabal/share/x86_64-linux-ghc-7.10.3/Exercise42-0.1.0.0"
libexecdir = "/home/ferdinand/.cabal/libexec"
sysconfdir = "/home/ferdinand/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Exercise42_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Exercise42_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Exercise42_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Exercise42_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Exercise42_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
