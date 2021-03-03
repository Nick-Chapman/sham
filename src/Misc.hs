module Misc (
  Block(..),
  EOF(..),
  EPIPE(..),
  NotReadable(..),
  NotWritable(..)
  ) where

data Block = Block
data EOF = EOF
data EPIPE = EPIPE
data NotReadable = NotReadable
data NotWritable = NotWritable
