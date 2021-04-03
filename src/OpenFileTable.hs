-- | System-wide table of 'open-files' which may be shared between different processes.
module OpenFileTable (
  init, OpenFileTable, Key,
  open, pipe, dup, close, read, write, ls, mv, rm, fileKind, loadBinary, whatIsKey,
  getTerminal
  ) where

import Data.List (intercalate)
import Data.Map (Map)
import FileSystem (FileSystem)
import Interaction (Interaction(..),EOF(..),Prompt(..),OutMode(..))
import Path (Path)
import PipeSystem (PipeSystem)
import Prelude hiding (init,read)
import Prog
import qualified Data.Map.Strict as Map
import qualified File
import qualified FileSystem
import qualified PipeSystem

data OpenFileTable = OpenFileTable
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

init :: FileSystem -> OpenFileTable
init fs = OpenFileTable
  { fs
  , pipeSystem = PipeSystem.empty
  , table = Tab $ Map.empty
  , nextKey = 21
  }

instance Show OpenFileTable where
  show OpenFileTable{fs=_,pipeSystem=ps,table,nextKey=_} =
    "open: " ++ show table ++ "\n" ++
    "pipe: " ++ show ps

getTerminal :: OpenFileTable -> OutMode -> (Key,OpenFileTable)
getTerminal state@OpenFileTable{nextKey=key,table} mode = do
  let nextKey = key+1
  let entry = Entry { rc = 1, what = Terminal mode }
  let table' = Tab (Map.insert key entry (unTab table))
  (key, state { nextKey, table = table' })

open :: OpenFileTable -> Path -> OpenMode -> Either OpenError (Key,OpenFileTable)
open state@OpenFileTable{nextKey=key,fs,table} path = \case
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


pipe :: OpenFileTable -> (PipeEnds Key,OpenFileTable)
pipe state@OpenFileTable{nextKey=key,pipeSystem,table} = do
  let (r,w,nextKey) = (key,key+1,key+2)
  let (pk,pipeSystem') = PipeSystem.createPipe pipeSystem
  let re = Entry { rc = 1, what = PipeRead pk}
  let we = Entry { rc = 1, what = PipeWrite pk }
  let table' = Tab (Map.insert r re (Map.insert w we (unTab table)))
  let state' = state { nextKey, table = table', pipeSystem = pipeSystem' }
  let ends = PipeEnds { r, w }
  (ends, state')


dup :: OpenFileTable -> Key -> OpenFileTable
dup state@OpenFileTable{table} key = do
  let e@Entry{rc} = look "dup" key (unTab table)
  let e' = e { rc = rc + 1 }
  let table' = Tab (Map.insert key e' (unTab table))
  state { table = table' }


close :: OpenFileTable -> Key -> OpenFileTable
close state0@OpenFileTable{pipeSystem,table} key = do
  let e@Entry{rc,what} = look "close" key (unTab table)
  let closing = (rc == 1)
  case closing of
    False -> do
      let e' = e { rc = rc - 1 }
      let table' = Tab (Map.insert key e' (unTab table))
      state0 { table = table' }
    True -> do
      let table' = Tab (Map.delete key (unTab table))
      let state = state0 { table = table' }
      case what of
        Terminal{} -> state
        PipeRead pk -> state { pipeSystem = PipeSystem.closeForReading pipeSystem pk }
        PipeWrite pk -> state { pipeSystem = PipeSystem.closeForWriting pipeSystem pk }
        FileAppend{} -> state
        FileContents{} -> state


type K a = (a -> Interaction) -> Interaction

read :: Prompt -> OpenFileTable -> Key -> K (Either NotReadable (Either Block (Either EOF String, OpenFileTable)))
read prompt state@OpenFileTable{table,pipeSystem} key k = do
  let e@Entry{what} = look "read" key (unTab table)
  case what of
    Terminal{} -> do
      I_Read prompt $ \case
        Nothing -> do
          k (Right (Left Block))
        Just lineOrEOF -> do
          k (Right (Right (lineOrEOF, state)))

    PipeWrite{} -> k (Left NotReadable)
    FileAppend{} -> k (Left NotReadable)
    PipeRead pk  -> k $ do
      case PipeSystem.readPipe pipeSystem pk of
        Left Block -> Right (Left Block)
        Right (x,pipeSystem) -> Right (Right (x,state {pipeSystem}))

    FileContents xs -> k $ do
      case xs of
        [] -> Right (Right (Left EOF, state))
        line:xs' -> do
          let what' = FileContents xs'
          let e' = e { what = what' }
          let table' = Tab (Map.insert key e' (unTab table))
          let state' = state { table = table' }
          Right (Right (Right line, state'))



write :: OpenFileTable -> Key -> String -> K (Either NotWritable (Either Block (Either EPIPE OpenFileTable)))
write state@OpenFileTable{table,pipeSystem,fs} key line k = do
  let Entry{what} = look "write" key (unTab table)
  case what of
    Terminal mode -> do
      I_Write mode line (k (Right (Right (Right state))))

    PipeRead{} -> k (Left NotWritable)
    FileContents{} -> k (Left NotWritable)
    PipeWrite pk -> k $ do
      case PipeSystem.writePipe pipeSystem pk line of
        Left Block -> Right (Left Block)
        Right (Left EPIPE) -> Right (Right (Left EPIPE))
        Right (Right pipeSystem) -> Right (Right (Right state { pipeSystem }))
    FileAppend path -> k $ do
      let file = case FileSystem.read fs path of
            Left NoSuchPath -> File.createData []
            Right file -> file
      case File.accessData file of
        Nothing -> Left NotWritable
        Just lines -> do
          let file' = File.createData (lines ++ [line])
          let fs' = FileSystem.link fs path file'
          Right (Right (Right state { fs = fs' }))

whatIsKey :: OpenFileTable -> Key -> OF
whatIsKey OpenFileTable{table} key = do
  let Entry{what} = look "whatIsKey" key (unTab table)
  what

ls :: OpenFileTable -> [Path]
ls OpenFileTable{fs} = FileSystem.ls fs

mv :: OpenFileTable -> Path -> Path -> Either NoSuchPath OpenFileTable
mv state@OpenFileTable{fs} src dest =
  case FileSystem.mv fs src dest of
    Left NoSuchPath -> Left NoSuchPath
    Right fs -> Right $ state { fs }

rm :: OpenFileTable -> Path -> Either NoSuchPath OpenFileTable
rm state@OpenFileTable{fs} path =
  case FileSystem.unlink fs path of
    Left NoSuchPath -> Left NoSuchPath
    Right fs -> Right $ state { fs }

fileKind :: OpenFileTable -> Path -> Either NoSuchPath FileKind
fileKind OpenFileTable{fs} path =
  case FileSystem.read fs path of
    Left NoSuchPath -> Left NoSuchPath
    Right file -> Right (File.kind file)

loadBinary :: OpenFileTable -> Path -> Either LoadBinaryError (Prog ())
loadBinary OpenFileTable{fs} path =
  case FileSystem.read fs path of
    Left NoSuchPath -> Left LBE_NoSuchPath
    Right file ->
      case File.accessProg file of
        Nothing -> Left LBE_CantLoadAsBinary
        Just prog -> Right prog

-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show (tag,k))) id (Map.lookup k env)
