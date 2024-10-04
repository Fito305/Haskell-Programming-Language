module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  -- The filter expression ensures that a listing for a single directory won't contain the special directory names . or ..,
  -- which refers to the current and parent directory, respectively. If we forgot to filter these out,
  -- we'd recurse endlessly.
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \names -> do
    let path = topdir </> names
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)
