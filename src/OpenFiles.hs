-- | System-wide table of 'open-files' which may be shared between different processes.
module OpenFiles (
  init, OpenFiles, Key,
  open, pipe, dup, close, read, write, ls, mv, fileKind, loadBinary, whatIsKey,
  ) where

import Data.List (intercalate)
import Data.Map (Map)
import FileSystem (FileSystem)
import Misc (Block(..),EOF(..),EPIPE(..),NotReadable(..),NotWritable(..),PipeEnds(..))
import Path (Path)
import PipeSystem (PipeSystem)
import Prelude hiding (init,read)
import Prog (Prog,OpenMode(..),WriteOpenMode(..),OpenError(..),LoadBinaryError(..),NoSuchPath(..),FileKind,OF(..))
import qualified Data.Map.Strict as Map
import qualified File
import qualified FileSystem
import qualified PipeSystem

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

type What = OF

data Entry = Entry { rc :: Int, what :: What }

instance Show Entry where
  show Entry{rc,what} =
    show what ++ "(" ++ show rc ++ ")"

init :: FileSystem -> OpenFiles
init fs = OpenFiles
  { fs
  , pipeSystem = PipeSystem.empty
  , table = Tab $ Map.fromList []
  , nextKey = 21
  }

instance Show OpenFiles where
  show OpenFiles{fs=_,pipeSystem=ps,table,nextKey=_} =
    "open: " ++ show table ++ "\n" ++
    "pipe: " ++ show ps


open :: OpenFiles -> Path -> OpenMode -> Either OpenError (Key,OpenFiles)
open state@OpenFiles{nextKey=key,fs,table} path = \case
  OpenForReading{} -> do
    let nextKey = key+1
    case FileSystem.read fs path of
      Left NoSuchPath -> Left OE_NoSuchPath
      Right file -> do
        case File.accessData file of
          Nothing -> Left OE_CantOpenForReading
          Just contents -> do
            let entry = Entry { rc = 1, what = FileContents contents }
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
        PipeRead pk -> state { pipeSystem = PipeSystem.closeForReading pipeSystem pk }
        PipeWrite pk -> state { pipeSystem = PipeSystem.closeForWriting pipeSystem pk }
        FileAppend{} -> state
        FileContents{} -> state)


read :: OpenFiles -> Key -> Either NotReadable (Either Block (Either EOF String, OpenFiles))
read state@OpenFiles{table,pipeSystem} key = do
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



write :: OpenFiles -> Key -> String -> Either NotWritable (Either Block (Either EPIPE OpenFiles))
write state@OpenFiles{table,pipeSystem,fs} key line = do
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
            Left NoSuchPath -> File.createData []
            Right file -> file
      case File.accessData file of
        Nothing -> Left NotWritable
        Just lines -> do
          let file' = File.createData (lines ++ [line])
          let fs' = FileSystem.link fs path file'
          Right (Right (Right state { fs = fs' }))

whatIsKey :: OpenFiles -> Key -> OF
whatIsKey OpenFiles{table} key = do
  let Entry{what} = look "whatIsKey" key (unTab table)
  what

ls :: OpenFiles -> [Path]
ls OpenFiles{fs} = FileSystem.ls fs

mv :: OpenFiles -> Path -> Path -> Either NoSuchPath OpenFiles
mv state@OpenFiles{fs} src dest =
  case FileSystem.mv fs src dest of
    Left NoSuchPath -> Left NoSuchPath
    Right fs -> Right $ state { fs }

fileKind :: OpenFiles -> Path -> Either NoSuchPath FileKind
fileKind OpenFiles{fs} path =
  case FileSystem.read fs path of
    Left NoSuchPath -> Left NoSuchPath
    Right file -> Right (File.kind file)

loadBinary :: OpenFiles -> Path -> Either LoadBinaryError (Prog ())
loadBinary OpenFiles{fs} path =
  case FileSystem.read fs path of
    Left NoSuchPath -> Left LBE_NoSuchPath
    Right file ->
      case File.accessProg file of
        Nothing -> Left LBE_CantLoadAsBinary
        Just prog -> Right prog

-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show (tag,k))) id (Map.lookup k env)
