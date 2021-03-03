
module FileTable (
  Fs,
  Fd,
  open, OpenMode(..),
  pipe, PipeEnds(..),
  dup,
  close,
  read, write, Block(..)
  ) where

import Prelude hiding (read)
import Path (Path)

data Fs -- includes: FileSystem, PipeSystem, and OpenFileTable

data Fd

data OpenMode
  = OpenForReading -- creating if doesn't exist
  | OpenForAppending -- creating if doesn't exist

data Block = Block

data PipeEnds = PipeEnds { readEnd :: Fd, writeEnd :: Fd }

open :: Fs -> Path -> OpenMode -> (Fd,Fs)
pipe :: Fs -> Fs -> (PipeEnds,Fs)
dup :: Fs -> Fd -> Fs
close :: Fs -> Fd -> Fs

read :: Fs -> Fd -> Either Block (Maybe String, Fs)
write :: Fs -> Fd -> String -> Either Block Fs

(open,pipe,dup,close,read,write) = undefined
