module Paths_relation_tool_examples (
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

bindir     = "/home/user8/Documents/programming/haskell/projects/MyStack/relation-tool-examples/.stack-work/install/x86_64-linux/lts-6.13/7.10.3/bin"
libdir     = "/home/user8/Documents/programming/haskell/projects/MyStack/relation-tool-examples/.stack-work/install/x86_64-linux/lts-6.13/7.10.3/lib/x86_64-linux-ghc-7.10.3/relation-tool-examples-0.1.0.0-DaZPwtpGOYDG2TMuXLK0RY"
datadir    = "/home/user8/Documents/programming/haskell/projects/MyStack/relation-tool-examples/.stack-work/install/x86_64-linux/lts-6.13/7.10.3/share/x86_64-linux-ghc-7.10.3/relation-tool-examples-0.1.0.0"
libexecdir = "/home/user8/Documents/programming/haskell/projects/MyStack/relation-tool-examples/.stack-work/install/x86_64-linux/lts-6.13/7.10.3/libexec"
sysconfdir = "/home/user8/Documents/programming/haskell/projects/MyStack/relation-tool-examples/.stack-work/install/x86_64-linux/lts-6.13/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "relation_tool_examples_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "relation_tool_examples_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "relation_tool_examples_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "relation_tool_examples_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "relation_tool_examples_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
