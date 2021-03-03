module Pipe (
  Pipe,
  W_Block(..),R_Block(..),
  empty,write,read
  ) where

import Prelude hiding (read)

data Pipe

empty :: Pipe

data W_Block = Full

write :: Pipe -> String -> Either W_Block Pipe

data R_Block = Empty

read :: Pipe -> Either R_Block (String,Pipe)

(empty,write,read) = undefined
