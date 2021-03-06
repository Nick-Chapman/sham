module OsState (
  init,
  OsState,
  Key,
  open, OpenMode(..), WriteOpenMode(..),
  pipe, PipeEnds(..),
  dup,
  close,
  Block(..),
  read, EOF(..),
  write, EPIPE(..),
  ls,
  ) where

import Data.Map (Map)
import FileSystem (FileSystem,NoSuchPath(..))
import Misc (Block(..),EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Path (Path)
import PipeSystem (PipeSystem,PipeKey)
import Prelude hiding (init,read)
import qualified Data.Map.Strict as Map
import qualified File (empty,append,lines)
import qualified FileSystem (ls,read,link,safeUnlink)
import qualified PipeSystem (empty,createPipe,readPipe,writePipe,closeForReading,closeForWriting)

data OsState = OsState
  { fs :: FileSystem
  , pipeSystem :: PipeSystem
  , table :: FileTable
  , nextKey :: Key
  }

newtype Key = Key Int
  -- These numbers are just keys into the open-file-table. Entirely *unrelated* to the
  -- per-process file-descriptor(index), stdin=0, stdout=1 etc
  deriving (Eq,Ord,Num,Show)

type FileTable = Map Key Entry
data Entry = Entry { rc :: Int, what :: What }

data What
  = PipeRead PipeKey
  | PipeWrite PipeKey
  | FileAppend Path -- nothing done when opened
  | FileContents [String] -- full contents read when opened


init :: FileSystem -> OsState
init fs = OsState
  { fs
  , pipeSystem = PipeSystem.empty
  , table = Map.empty
  , nextKey = 100
  }

data OpenMode
  = OpenForReading -- creating if doesn't exist
  | OpenForWriting WriteOpenMode -- rm, then append
  deriving Show

data WriteOpenMode = Truncate | Append deriving Show

open :: OsState -> Path -> OpenMode -> Either NoSuchPath (Key,OsState)
open state@OsState{nextKey=key,fs,table} path = \case
  OpenForReading{} -> do
    let nextKey = key+1
    case FileSystem.read fs path of
      Left NoSuchPath -> Left NoSuchPath
      Right file -> do
        let entry = Entry { rc = 1, what = FileContents (File.lines file) }
        let table' = Map.insert key entry table
        let state' = state { nextKey, table = table' }
        Right (key,state')
  OpenForWriting wom -> do
    let
      fs' = case wom of
        Truncate -> FileSystem.safeUnlink fs path
        Append -> fs
    let nextKey = key+1
    let entry = Entry { rc = 1, what = FileAppend path }
    let table' = Map.insert key entry table
    let state' = state { nextKey, fs = fs', table = table' }
    Right (key,state')


data PipeEnds = PipeEnds { r :: Key, w :: Key }

pipe :: OsState -> (PipeEnds,OsState)
pipe state@OsState{nextKey=key,pipeSystem,table} = do
  let (r,w,nextKey) = (key,key+1,key+2)
  let (pk,pipeSystem') = PipeSystem.createPipe pipeSystem
  let re = Entry { rc = 1, what = PipeRead pk}
  let we = Entry { rc = 1, what = PipeWrite pk }
  let table' = Map.insert r re (Map.insert w we table)
  let state' = state { nextKey, table = table', pipeSystem = pipeSystem' }
  let ends = PipeEnds { r, w }
  (ends, state')


dup :: OsState -> Key -> OsState
dup state@OsState{table} key = do
  let e@Entry{rc} = look "dup" key table
  let e' = e { rc = rc + 1 }
  let table' = Map.insert key e' table
  state { table = table' }


close :: OsState -> Key -> OsState
close state0@OsState{pipeSystem,table} key = do
  let e@Entry{rc,what} = look "close" key table
  case rc > 1 of
    True -> do
      let e' = e { rc = rc + 1 }
      let table' = Map.insert key e' table
      state0 { table = table' }
    False -> do
      let table' = Map.delete key table
      let state = state0 { table = table' }
      case what of
        PipeRead pk -> state { pipeSystem = PipeSystem.closeForReading pipeSystem pk }
        PipeWrite pk -> state { pipeSystem = PipeSystem.closeForWriting pipeSystem pk }
        FileAppend{} -> state
        FileContents{} -> state


read :: OsState -> Key -> Either NotReadable (Either Block (Either EOF String, OsState))
read state@OsState{table,pipeSystem} key = do
  let e@Entry{what} = look "write" key table
  case what of
    PipeWrite{} -> Left NotReadable
    FileAppend{} -> Left NotReadable
    PipeRead pk  -> do
      case PipeSystem.readPipe pipeSystem pk of
        Left Block -> Right (Left Block)
        Right (Left EOF) -> Right (Right (Left EOF, state))
        Right (Right (line,pipeSystem)) -> do
          let state' = state { pipeSystem  }
          Right (Right (Right line, state'))
    FileContents xs -> do
      case xs of
        [] -> Right (Right (Left EOF, state))
        line:xs' -> do
          let what' = FileContents xs'
          let e' = e { what = what' }
          let table' = Map.insert key e' table
          let state' = state { table = table' }
          Right (Right (Right line, state'))



write :: OsState -> Key -> String -> Either NotWritable (Either Block (Either EPIPE OsState))
write state@OsState{table,pipeSystem,fs} key line = do
  let Entry{what} = look "write" key table
  case what of
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


ls :: OsState -> [Path]
ls OsState{fs} = FileSystem.ls fs

-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show (tag,k))) id (Map.lookup k env)
