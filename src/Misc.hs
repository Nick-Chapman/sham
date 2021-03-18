-- | Various types used at different system levels.
module Misc (
  Block(..),
  EOF(..),
  EPIPE(..),
  NotReadable(..),
  NotWritable(..),
  PipeEnds(..),
  ) where

-- TODO: move these types in Prog?

data Block = Block
data EOF = EOF deriving Show
data EPIPE = EPIPE deriving Show
data NotReadable = NotReadable deriving Show
data NotWritable = NotWritable deriving Show
data PipeEnds a = PipeEnds { r :: a, w :: a } deriving Show
