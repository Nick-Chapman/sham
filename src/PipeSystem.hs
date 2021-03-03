module PipeSystem (
  PipeSystem,
  empty,
  createPipe, PipeKey,
  destroyPipe,
  writePipe, W_Fail(..),
  readPipe, R_Fail(..),OrEOF(..)
  ) where

data PipeSystem

data PipeKey

data W_Fail = W_FullSoBlock | W_ReaderGoneAway

data OrEOF a = EOF | NotEOF a

data R_Fail = R_EmptySoBlock

empty :: PipeSystem
createPipe :: PipeSystem -> (PipeKey, PipeSystem)
destroyPipe :: PipeSystem -> PipeKey -> PipeSystem
writePipe :: PipeSystem -> PipeKey -> String -> Either W_Fail PipeSystem
readPipe :: PipeSystem -> PipeKey -> Either R_Fail (OrEOF String,PipeSystem)

(empty,createPipe,destroyPipe,writePipe,readPipe) = undefined
