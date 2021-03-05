module Misc (
  Block(..),
  EOF(..),
  EPIPE(..),
  NotReadable(..),
  NotWritable(..)
  ) where

data Block = Block
data EOF = EOF deriving Show
data EPIPE = EPIPE deriving Show
data NotReadable = NotReadable deriving Show
data NotWritable = NotWritable deriving Show
