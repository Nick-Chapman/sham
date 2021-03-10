module OsState (  -- TODO: rename OpenFiles?
  init,
  OsState,
  Key,
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
  = PipeRead PipeKey
  | PipeWrite PipeKey
  | FileAppend Path -- nothing done when opened
  | FileContents [String] -- full contents read when opened

instance Show What where
  show = \case
    PipeRead pk -> "Read:"++show pk
    PipeWrite pk -> "Write:"++show pk
    FileAppend path -> "Append:"++show path
    FileContents xs -> "Contents[size=#"++show (length xs)++"]"

init :: FileSystem -> OsState
init fs = OsState
  { fs
  , pipeSystem = PipeSystem.empty
  , table = Tab Map.empty
  , nextKey = 21
  }

instance Show OsState where
  show OsState{fs=_,pipeSystem=ps,table,nextKey=_} =
    "open: " ++ show table ++ "\n" ++
    "pipe: " ++ show ps

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


pipe :: OsState -> (PipeEnds Key,OsState)
pipe state@OsState{nextKey=key,pipeSystem,table} = do
  let (r,w,nextKey) = (key,key+1,key+2)
  let (pk,pipeSystem') = PipeSystem.createPipe pipeSystem
  let re = Entry { rc = 1, what = PipeRead pk}
  let we = Entry { rc = 1, what = PipeWrite pk }
  let table' = Tab (Map.insert r re (Map.insert w we (unTab table)))
  let state' = state { nextKey, table = table', pipeSystem = pipeSystem' }
  let ends = PipeEnds { r, w }
  (ends, state')


dup :: OsState -> Key -> OsState
dup state@OsState{table} key = do
  let e@Entry{rc} = look "dup" key (unTab table)
  let e' = e { rc = rc + 1 }
  let table' = Tab (Map.insert key e' (unTab table))
  state { table = table' }


close :: OsState -> Key -> (Bool,OsState)
close state0@OsState{pipeSystem,table} key = do
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
        PipeRead pk -> state { pipeSystem = PipeSystem.closeForReading pipeSystem pk }
        PipeWrite pk -> state { pipeSystem = PipeSystem.closeForWriting pipeSystem pk }
        FileAppend{} -> state
        FileContents{} -> state)


read :: OsState -> Key -> Either NotReadable (Either Block (Either EOF String, OsState))
read state@OsState{table,pipeSystem} key = do
  let e@Entry{what} = look "read" key (unTab table)
  case what of
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



write :: OsState -> Key -> String -> Either NotWritable (Either Block (Either EPIPE OsState))
write state@OsState{table,pipeSystem,fs} key line = do
  let Entry{what} = look "write" key (unTab table)
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
