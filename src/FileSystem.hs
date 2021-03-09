module FileSystem (
  FileSystem,
  create,
  ls,
  link,
  exists,
  read, unlink, NoSuchPath(..),
  safeUnlink,
  fs0, readme, days
  ) where

import Data.Map (Map)
import File (File)
import Path (Path)
import Prelude hiding (read,words)
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
  [ (Path.create "README", File.create readme)
  , (Path.create "days", File.create days)
  , (Path.create "help", File.create ["cat README"])
  , (Path.create "y", File.create ["echo yes","y &"])
  ]

readme :: [String]
readme =
  [ "Welcome to Nick's simulated bash."
  , "Some available commands: bins, echo, cat, rev, grep, ls, ps, xargs, exit."
  , "For more help, perhaps run: bins | xargs man"
  ]

days :: [String]
days = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
