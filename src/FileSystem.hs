module FileSystem (
  FileSystem,
  create,
  ls,
  link,
  exists,
  read, unlink, NoSuchPath(..),
  safeUnlink,
  fs0
  ) where

import Data.Map (Map)
import File (File)
import Path (Path)
import Prelude hiding (read)
import qualified Data.Map.Strict as Map
import qualified Path (create)
import qualified File (create)

data NoSuchPath = NoSuchPath deriving Show

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

fs0 :: FileSystem
fs0 = FileSystem.create
  [ (Path.create "words", File.create ["one","two","three"])
  , (Path.create "test", File.create ["rev < words >> rw", "cat rw"])
  , (Path.create "oe", File.create ["this is wrong", "echo to-out", "echo to-err 1>>&2"])
  , (Path.create "t1", File.create ["oe >> xx"])
  , (Path.create "t2", File.create ["oe 2>> yy"])
  , (Path.create "t3", File.create ["oe > xx"])
  , (Path.create "t4", File.create ["cat words &", "cat words"])
  , (Path.create "t", File.create ["cat &", "rev"])
  ]
