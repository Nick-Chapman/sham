-- | 'pipefs' is the ephemeral file-system mapping pipe-keys to pipes.
module PipeSystem (
  PipeSystem,
  empty,
  createPipe, Key,
  writePipe,
  readPipe,
  closeForReading, closeForWriting,
  ) where

import Data.List (intercalate)
import Data.Map (Map)
import Misc (Block(..),EOF(..),EPIPE(..))
import Pipe (Pipe)
import Prog (PipeKey)
import qualified Data.Map.Strict as Map
import qualified Pipe

empty :: PipeSystem
createPipe :: PipeSystem -> (Key, PipeSystem)
writePipe :: PipeSystem -> Key -> String -> Either Block (Either EPIPE PipeSystem)
readPipe :: PipeSystem -> Key -> Either Block (Either EOF String,PipeSystem)
closeForReading :: PipeSystem -> Key -> PipeSystem
closeForWriting :: PipeSystem -> Key -> PipeSystem

type Key = PipeKey

data PipeSystem = State
  { m :: Map Key (Pipe,Mode)
  , next :: Key
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

empty = State { m = Map.empty, next = 1 }

createPipe s@State{m,next=key} = do
  let pipe = Pipe.empty
  (key, s { m = Map.insert key (pipe,Active) m, next = key + 1 })

writePipe s@State{m} k str = do
  let (pipe,mode) = look "writePipe" k m
  case mode of
    Drain -> error "writing to a draining pipe (closed for writing)"
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
        Drain -> Right (Left EOF, s)
        Unwatched ->
          error "reading from an unwatched pipe (closed for reading)"
    Right (str,pipe) ->
      Right (Right str, s { m = Map.insert k (pipe,mode) m })

closeForReading s@State{m} k = do
  let (pipe,mode) = look "closeForReading" k m
  case mode of
    Active -> s { m = Map.insert k (pipe,Unwatched) m }
    Drain -> s { m = Map.delete k m }
    Unwatched ->
      error "already closed for reading"

closeForWriting s@State{m} k = do
  let (pipe,mode) = look "closeForWriting" k m
  case mode of
    Active -> s { m = Map.insert k (pipe,Drain) m }
    Drain -> error "already closed for writing"
    Unwatched -> s { m = Map.delete k m }


look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show (tag,k))) id (Map.lookup k env)
