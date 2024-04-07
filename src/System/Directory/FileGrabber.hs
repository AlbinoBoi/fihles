module System.Directory.FileGrabber where

import System.Directory

getDirectoryEntries :: IO [FilePath]
getDirectoryEntries = getCurrentDirectory >>= listDirectory