module Pipe (
  Pipe,
  W_Block(..),R_Block(..),
  empty,write,read
  ) where

import Prelude hiding (read)

empty :: Pipe
write :: Pipe -> String -> Either W_Block Pipe
read :: Pipe -> Either R_Block (String,Pipe)

data R_Block = Empty  -- TODO: use Misc.Block
data W_Block = Full -- TODO: use Misc.Block

{-data Pipe = Pipe [String] -- unbounded pipe
instance Show Pipe where show (Pipe xs) = show xs
empty = Pipe []
write (Pipe xs) x = Right (Pipe (xs ++ [x]))
read (Pipe []) = Left Empty
read (Pipe (x:xs)) = Right (x, Pipe xs)-}

data Pipe = Pipe (Maybe String) -- 0 or 1 element
empty = Pipe Nothing
instance Show Pipe where show (Pipe m) = show m
write (Pipe Nothing) x = Right (Pipe (Just x))
write (Pipe (Just _)) _ = Left Full
read (Pipe Nothing) = Left Empty
read (Pipe (Just x)) = Right (x, Pipe Nothing)

