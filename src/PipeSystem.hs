module PipeSystem (
  PipeSystem,
  empty,
  createPipe, PipeKey,
  writePipe,
  readPipe,
  closeForReading, closeForWriting,
  ) where

import Data.List (intercalate)
import Data.Map (Map)
import Misc (Block(..),EOF(..),EPIPE(..))
import Pipe (Pipe)
import qualified Data.Map.Strict as Map
import qualified Pipe

empty :: PipeSystem
createPipe :: PipeSystem -> (PipeKey, PipeSystem)
writePipe :: PipeSystem -> PipeKey -> String -> Either Block (Either EPIPE PipeSystem)
readPipe :: PipeSystem -> PipeKey -> Either Block (Either EOF String,PipeSystem)
closeForReading :: PipeSystem -> PipeKey -> PipeSystem
closeForWriting :: PipeSystem -> PipeKey -> PipeSystem

data PipeSystem = State
  { m :: Map PipeKey (Pipe,Mode)
  , next :: PipeKey
  }

data Mode = Active | Drain | Unwatched
instance Show Mode where
  show = \case
    Active -> "(active)"
    Drain -> "(draining)"
    Unwatched -> "(unwatched)"

instance Show PipeSystem where
  show State{m} =
    intercalate "," [ show k ++ show m ++ show p | (k,(p,m)) <- Map.toList m ]

newtype PipeKey = PK Int deriving (Eq,Ord,Num) -- TODO: rename Key
instance Show PipeKey where show (PK n) = "pipe"++show n

empty = State { m = Map.empty, next = 1 }

createPipe s@State{m,next=key} = do
  let pipe = Pipe.empty
  (key, s { m = Map.insert key (pipe,Active) m, next = key + 1 })

writePipe s@State{m} k str = do
  let (pipe,mode) = look "writePipe" k m
  case mode of
    Drain -> error "writing to a draining pipe (closed for writing); should be impossible"
    Unwatched -> Right (Left EPIPE)
    Active -> do
      case Pipe.write pipe str of
        Left Block -> Left Block
        Right pipe -> Right (Right s { m = Map.insert k (pipe,mode) m })

readPipe s@State{m} k = do
  let (pipe,mode) = look "readPipe" k m
  case Pipe.read pipe of
    Left Block ->
      case mode of
        Active -> Left Block
        Drain -> Right (Left EOF, s { m = Map.delete k m }) --TODO: here or when closeForReading
        Unwatched -> error "reading from an unwatched pipe (closed for reading); should be impossible"
    Right (str,pipe) ->
      Right (Right str, s { m = Map.insert k (pipe,mode) m })

closeForReading s@State{m} k = do
  case Map.lookup k m of
    Nothing -> s --TODO: impossible if we only delete when both ends are closed
    Just (pipe,mode) ->
      case mode of
        Active -> s { m = Map.insert k (pipe,Unwatched) m }
        Drain -> s
        Unwatched -> error "already closed for reading; should be impossible"

closeForWriting s@State{m} k = do
  let (pipe,mode) = look "closeForWriting" k m
  case mode of
    Active -> s { m = Map.insert k (pipe,Drain) m }
    Drain -> error "already closed for writing; should be impossible"
    Unwatched -> s -- TODO: delete here?

-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show (tag,k))) id (Map.lookup k env)
