-- | Simple flat file-system. Mapping 'path' to 'file' (contents).
module FileSystem (
  create, FileSystem,
  ls,
  mv,
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
mv :: FileSystem -> Path -> Path -> Either NoSuchPath FileSystem
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

mv fs src dest =
  case Map.lookup src fs of
    Nothing -> Left NoSuchPath
    Just file -> Right $ (Map.delete src . Map.insert dest file) fs
