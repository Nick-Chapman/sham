module Pipe (
  Pipe,
  empty,write,read
  ) where

import Misc (Block(..))
import Prelude hiding (read)

empty :: Pipe
write :: Pipe -> String -> Either Block Pipe
read :: Pipe -> Either Block (String,Pipe)

{-data Pipe = Pipe [String] -- unbounded pipe
instance Show Pipe where show (Pipe xs) = show xs
empty = Pipe []
write (Pipe xs) x = Right (Pipe (xs ++ [x]))
read (Pipe []) = Left Block
read (Pipe (x:xs)) = Right (x, Pipe xs)-}

data Pipe = Pipe (Maybe String) -- 0 or 1 element
empty = Pipe Nothing
instance Show Pipe where show (Pipe m) = show m
write (Pipe Nothing) x = Right (Pipe (Just x))
write (Pipe (Just _)) _ = Left Block
read (Pipe Nothing) = Left Block
read (Pipe (Just x)) = Right (x, Pipe Nothing)

