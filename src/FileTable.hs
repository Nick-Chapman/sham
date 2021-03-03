
module FileTable (
  empty,
  Fs,
  Key,
  open, OpenMode(..),
  pipe, PipeEnds(..),
  dup,
  close,
  Block(..),
  read, EOF(..),
  write, EPIPE(..),
  ) where

import Data.Map (Map)
import FileSystem (FileSystem,NoSuchPath(..))
import Misc (Block(..),EOF(..),EPIPE(..))
import Path (Path)
import PipeSystem (PipeSystem,PipeKey)
import Prelude hiding (read)
import qualified Data.Map.Strict as Map
import qualified File (empty,append,lines)
import qualified FileSystem (empty,read,link)
import qualified PipeSystem (empty,createPipe,readPipe,writePipe,closeForReading,closeForWriting)


data Fs = Fs
  { fileSystem :: FileSystem
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


empty :: Fs
empty = Fs
  { fileSystem = FileSystem.empty
  , pipeSystem = PipeSystem.empty
  , table = Map.empty
  , nextKey = 100
  }


data OpenMode
  = OpenForReading -- creating if doesn't exist
  | OpenForAppending -- creating if doesn't exist

open :: Fs -> Path -> OpenMode -> Either NoSuchPath (Key,Fs)
open fs@Fs{nextKey=key,fileSystem,table} path = \case
  OpenForReading{} -> do
    let nextKey = key+1
    case FileSystem.read fileSystem path of
      Left NoSuchPath -> Left NoSuchPath
      Right file -> do
        let entry = Entry { rc = 1, what = FileContents (File.lines file) }
        let table' = Map.insert key entry table
        let fs' = fs { nextKey, table = table' }
        Right (key,fs')
  OpenForAppending{} -> do
    let nextKey = key+1
    -- fileSystem not accessed here
    -- we dont care if the path doesn't exist now, it will be create when writing
    let entry = Entry { rc = 1, what = FileAppend path }
    let table' = Map.insert key entry table
    let fs' = fs { nextKey, table = table' }
    Right (key,fs')


data PipeEnds = PipeEnds { r :: Key, w :: Key }

pipe :: Fs -> (PipeEnds,Fs)
pipe fs@Fs{nextKey=key,pipeSystem,table} = do
  let (r,w,nextKey) = (key,key+1,key+2)
  let (pk,pipeSystem') = PipeSystem.createPipe pipeSystem
  let re = Entry { rc = 1, what = PipeRead pk}
  let we = Entry { rc = 1, what = PipeWrite pk }
  let table' = Map.insert r re (Map.insert w we table)
  let fs' = fs { nextKey, table = table', pipeSystem = pipeSystem' }
  let ends = PipeEnds { r, w }
  (ends, fs')


dup :: Fs -> Key -> Fs
dup fs@Fs{table} key = do
  let e@Entry{rc} = look "dup" key table
  let e' = e { rc = rc + 1 }
  let table' = Map.insert key e' table
  fs { table = table' }


close :: Fs -> Key -> Fs
close fs0@Fs{pipeSystem,table} key = do
  let e@Entry{rc,what} = look "close" key table
  case rc > 1 of
    True -> do
      let e' = e { rc = rc + 1 }
      let table' = Map.insert key e' table
      fs0 { table = table' }
    False -> do
      let table' = Map.delete key table
      let fs = fs0 { table = table' }
      case what of
        PipeRead pk -> fs { pipeSystem = PipeSystem.closeForReading pipeSystem pk }
        PipeWrite pk -> fs { pipeSystem = PipeSystem.closeForWriting pipeSystem pk }
        FileAppend{} -> fs
        FileContents{} -> fs


data NotReadable = NotReadable

read :: Fs -> Key -> Either NotReadable (Either Block (Either EOF String, Fs))
read fs@Fs{table,pipeSystem} key = do
  let e@Entry{what} = look "write" key table
  case what of
    PipeWrite{} -> Left NotReadable
    FileAppend{} -> Left NotReadable
    PipeRead pk  -> do
      case PipeSystem.readPipe pipeSystem pk of
        Left Block -> Right (Left Block)
        Right (Left EOF) -> Right (Right (Left EOF, fs))
        Right (Right (line,pipeSystem)) -> do
          let fs' = fs { pipeSystem  }
          Right (Right (Right line, fs'))
    FileContents xs -> do
      case xs of
        [] -> Right (Right (Left EOF, fs))
        line:xs' -> do
          let what' = FileContents xs'
          let e' = e { what = what' }
          let table' = Map.insert key e' table
          let fs' = fs { table = table' }
          Right (Right (Right line, fs'))


data NotWritable = NotWritable

write :: Fs -> Key -> String -> Either NotWritable (Either Block (Either EPIPE Fs))
write fs@Fs{table,pipeSystem,fileSystem} key line = do
  let Entry{what} = look "write" key table
  case what of
    PipeRead{} -> Left NotWritable
    FileContents{} -> Left NotWritable
    PipeWrite pk ->
      case PipeSystem.writePipe pipeSystem pk line of
        Left Block -> Right (Left Block)
        Right (Left EPIPE) -> Right (Right (Left EPIPE))
        Right (Right pipeSystem) -> Right (Right (Right fs { pipeSystem }))
    FileAppend path -> do
      let file = case FileSystem.read fileSystem path of
            Left NoSuchPath -> File.empty
            Right file -> file
      let file' = File.append file line
      let fileSystem' = FileSystem.link fileSystem path file'
      Right (Right (Right fs { fileSystem = fileSystem' }))


-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show (tag,k))) id (Map.lookup k env)
