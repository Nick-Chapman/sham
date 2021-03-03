module FileSystem (
  FileSystem,
  empty,
  link,
  exists,
  read, unlink, NoSuchPath(..),
  ) where

import Prelude hiding (read)
import Path (Path)
import File (File)

data FileSystem

data NoSuchPath = NoSuchPath

empty :: FileSystem
link :: FileSystem -> Path -> File -> FileSystem
exists :: FileSystem -> Path -> Bool
read :: FileSystem -> Path -> Either NoSuchPath File
unlink :: FileSystem -> Path -> Either NoSuchPath FileSystem

(empty,link,unlink,exists,read) = undefined
