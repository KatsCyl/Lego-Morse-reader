module Paths_massive_ironman (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kats/Projects/robo/massive-ironman/.cabal-sandbox/bin"
libdir     = "/home/kats/Projects/robo/massive-ironman/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.3/massive-ironman-0.1.0.0"
datadir    = "/home/kats/Projects/robo/massive-ironman/.cabal-sandbox/share/x86_64-linux-ghc-7.8.3/massive-ironman-0.1.0.0"
libexecdir = "/home/kats/Projects/robo/massive-ironman/.cabal-sandbox/libexec"
sysconfdir = "/home/kats/Projects/robo/massive-ironman/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "massive_ironman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "massive_ironman_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "massive_ironman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "massive_ironman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "massive_ironman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
