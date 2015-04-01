module Paths_cis194_intro_to_haskell (
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

bindir     = "/Users/ptrckbrwn/src/cis194-intro_to_haskell/.cabal-sandbox/bin"
libdir     = "/Users/ptrckbrwn/src/cis194-intro_to_haskell/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.4/cis194-intro-to-haskell-0.1.0.0"
datadir    = "/Users/ptrckbrwn/src/cis194-intro_to_haskell/.cabal-sandbox/share/x86_64-osx-ghc-7.8.4/cis194-intro-to-haskell-0.1.0.0"
libexecdir = "/Users/ptrckbrwn/src/cis194-intro_to_haskell/.cabal-sandbox/libexec"
sysconfdir = "/Users/ptrckbrwn/src/cis194-intro_to_haskell/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cis194_intro_to_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cis194_intro_to_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cis194_intro_to_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cis194_intro_to_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cis194_intro_to_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
