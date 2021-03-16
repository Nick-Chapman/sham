module FileSystem (
  create, FileSystem,
  ls,
  link,
  exists,
  read, unlink,
  safeUnlink,
  ) where

import Data.Map (Map)
import File (File)
import Path (Path)
import Prelude hiding (read,words)
import Prog (NoSuchPath(..))
import qualified Data.Map.Strict as Map

create :: [(Path,File)] -> FileSystem
ls :: FileSystem -> [Path]
link :: FileSystem -> Path -> File -> FileSystem
exists :: FileSystem -> Path -> Bool
read :: FileSystem -> Path -> Either NoSuchPath File
unlink :: FileSystem -> Path -> Either NoSuchPath FileSystem
safeUnlink :: FileSystem -> Path -> FileSystem

type FileSystem = Map Path File
create = Map.fromList
ls = Map.keys
link fs path file = Map.insert path file fs
exists fs path = Map.member path fs
read fs path = maybe (Left NoSuchPath) Right (Map.lookup path fs)
unlink fs path = if exists fs path then Right (Map.delete path fs) else Left NoSuchPath
safeUnlink fs path = Map.delete path fs
