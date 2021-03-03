module PipeSystem (
  PipeSystem,
  empty,
  createPipe, PipeKey,
  writePipe,
  readPipe,
  closeForReading, closeForWriting,
  ) where

import Misc (Block(..),EOF(..),EPIPE(..))

data PipeSystem

data PipeKey

empty :: PipeSystem
createPipe :: PipeSystem -> (PipeKey, PipeSystem)
writePipe :: PipeSystem -> PipeKey -> String -> Either Block (Either EPIPE PipeSystem)
readPipe :: PipeSystem -> PipeKey -> Either Block (Either EOF (String,PipeSystem))

closeForReading :: PipeSystem -> PipeKey -> PipeSystem
closeForWriting :: PipeSystem -> PipeKey -> PipeSystem

(empty,createPipe,writePipe,readPipe,closeForReading,closeForWriting) = undefined
