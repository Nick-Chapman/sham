module OpenFiles (
  init,
  OpenFiles,
  Key,
  devnull,
  open, OpenMode(..), WriteOpenMode(..),
  pipe,
  dup,
  close,
  Block(..),
  read, EOF(..),
  write, EPIPE(..),
  ls,
  ) where

import Data.List (intercalate)
import Data.Map (Map)
import FileSystem (FileSystem,NoSuchPath(..))
import Misc (Block(..),EOF(..),EPIPE(..),NotReadable(..),NotWritable(..),PipeEnds(..))
import Path (Path)
import PipeSystem (PipeSystem)
import Prelude hiding (init,read)
import qualified Data.Map.Strict as Map
import qualified File (empty,append,lines)
import qualified FileSystem (ls,read,link,safeUnlink)
import qualified PipeSystem (Key,empty,createPipe,readPipe,writePipe,closeForReading,closeForWriting)

data OpenFiles = OpenFiles
  { fs :: FileSystem
  , pipeSystem :: PipeSystem
  , table :: FileTable
  , nextKey :: Key
  }

newtype Key = Key Int
  -- These numbers are just keys into the open-file-table. Entirely *unrelated* to the
  -- per-process file-descriptor(index), stdin=0, stdout=1 etc
  deriving (Eq,Ord,Num)

instance Show Key where show (Key n) = "k"++show n

newtype FileTable = Tab { unTab :: Map Key Entry }

instance Show FileTable where
  show Tab{unTab=m} =
    intercalate ", " [ show k ++ "=" ++ show e | (k,e) <- Map.toList m ]

data Entry = Entry { rc :: Int, what :: What }

instance Show Entry where
  show Entry{rc,what} =
    show what ++ "(" ++ show rc ++ ")"

data What
  = PipeRead PipeSystem.Key
  | PipeWrite PipeSystem.Key
  | FileAppend Path -- nothing done when opened
  | FileContents [String] -- full contents read when opened
  | DevNull

instance Show What where
  show = \case
    PipeRead pk -> "Read:"++show pk
    PipeWrite pk -> "Write:"++show pk
    FileAppend path -> "Append:"++show path
    FileContents xs -> "Contents[size=#"++show (length xs)++"]"
    DevNull -> "/dev/null"

init :: FileSystem -> OpenFiles
init fs = OpenFiles
  { fs
  , pipeSystem = PipeSystem.empty
  , table = Tab $ Map.fromList [(21, Entry { rc = 1, what = DevNull })]
  , nextKey = 22
  }

devnull :: Key
devnull = 21

instance Show OpenFiles where
  show OpenFiles{fs=_,pipeSystem=ps,table,nextKey=_} =
    "open: " ++ show table ++ "\n" ++
    "pipe: " ++ show ps

data OpenMode
  = OpenForReading -- creating if doesn't exist
  | OpenForWriting WriteOpenMode -- rm, then append
  deriving Show

data WriteOpenMode = Truncate | Append deriving Show

open :: OpenFiles -> Path -> OpenMode -> Either NoSuchPath (Key,OpenFiles)
open state@OpenFiles{nextKey=key,fs,table} path = \case
  OpenForReading{} -> do
    let nextKey = key+1
    case FileSystem.read fs path of
      Left NoSuchPath -> Left NoSuchPath
      Right file -> do
        let entry = Entry { rc = 1, what = FileContents (File.lines file) }
        let table' = Tab (Map.insert key entry (unTab table))
        let state' = state { nextKey, table = table' }
        Right (key,state')
  OpenForWriting wom -> do
    let
      fs' = case wom of
        Truncate -> FileSystem.safeUnlink fs path
        Append -> fs
    let nextKey = key+1
    let entry = Entry { rc = 1, what = FileAppend path }
    let table' = Tab (Map.insert key entry (unTab table))
    let state' = state { nextKey, fs = fs', table = table' }
    Right (key,state')


pipe :: OpenFiles -> (PipeEnds Key,OpenFiles)
pipe state@OpenFiles{nextKey=key,pipeSystem,table} = do
  let (r,w,nextKey) = (key,key+1,key+2)
  let (pk,pipeSystem') = PipeSystem.createPipe pipeSystem
  let re = Entry { rc = 1, what = PipeRead pk}
  let we = Entry { rc = 1, what = PipeWrite pk }
  let table' = Tab (Map.insert r re (Map.insert w we (unTab table)))
  let state' = state { nextKey, table = table', pipeSystem = pipeSystem' }
  let ends = PipeEnds { r, w }
  (ends, state')


dup :: OpenFiles -> Key -> OpenFiles
dup state@OpenFiles{table} key = do
  let e@Entry{rc} = look "dup" key (unTab table)
  let e' = e { rc = rc + 1 }
  let table' = Tab (Map.insert key e' (unTab table))
  state { table = table' }


close :: OpenFiles -> Key -> (Bool,OpenFiles)
close state0@OpenFiles{pipeSystem,table} key = do
  let e@Entry{rc,what} = look "close" key (unTab table)
  let closing = (rc == 1)
  (closing, case closing of
    False -> do
      let e' = e { rc = rc - 1 }
      let table' = Tab (Map.insert key e' (unTab table))
      state0 { table = table' }
    True -> do
      let table' = Tab (Map.delete key (unTab table))
      let state = state0 { table = table' }
      case what of
        DevNull -> state
        PipeRead pk -> state { pipeSystem = PipeSystem.closeForReading pipeSystem pk }
        PipeWrite pk -> state { pipeSystem = PipeSystem.closeForWriting pipeSystem pk }
        FileAppend{} -> state
        FileContents{} -> state)


read :: OpenFiles -> Key -> Either NotReadable (Either Block (Either EOF String, OpenFiles))
read state@OpenFiles{table,pipeSystem} key = do
  let e@Entry{what} = look "read" key (unTab table)
  case what of
    DevNull -> Right (Right (Left EOF, state))
    PipeWrite{} -> Left NotReadable
    FileAppend{} -> Left NotReadable
    PipeRead pk  -> do
      case PipeSystem.readPipe pipeSystem pk of
        Left Block -> Right (Left Block)
        Right (x,pipeSystem) -> Right (Right (x,state {pipeSystem}))

    FileContents xs -> do
      case xs of
        [] -> Right (Right (Left EOF, state))
        line:xs' -> do
          let what' = FileContents xs'
          let e' = e { what = what' }
          let table' = Tab (Map.insert key e' (unTab table))
          let state' = state { table = table' }
          Right (Right (Right line, state'))



write :: OpenFiles -> Key -> String -> Either NotWritable (Either Block (Either EPIPE OpenFiles))
write state@OpenFiles{table,pipeSystem,fs} key line = do
  let Entry{what} = look "write" key (unTab table)
  case what of
    DevNull -> Right (Right (Right state))
    PipeRead{} -> Left NotWritable
    FileContents{} -> Left NotWritable
    PipeWrite pk ->
      case PipeSystem.writePipe pipeSystem pk line of
        Left Block -> Right (Left Block)
        Right (Left EPIPE) -> Right (Right (Left EPIPE))
        Right (Right pipeSystem) -> Right (Right (Right state { pipeSystem }))
    FileAppend path -> do
      let file = case FileSystem.read fs path of
            Left NoSuchPath -> File.empty
            Right file -> file
      let file' = File.append file line
      let fs' = FileSystem.link fs path file'
      Right (Right (Right state { fs = fs' }))


ls :: OpenFiles -> [Path]
ls OpenFiles{fs} = FileSystem.ls fs

-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show (tag,k))) id (Map.lookup k env)
