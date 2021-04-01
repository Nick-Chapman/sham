-- | A 'program' which can be executed by MeNicks.
module Prog (
  Prog(..), SysCall(..), FileKind(..),BinaryMeta(..),
  OpenMode(..), WriteOpenMode(..),
  BadFileDescriptor(..), OpenError(..), LoadBinaryError(..), NoSuchPath(..),
  Command(..), FD(..), Pid(..),
  PipeKey, OF(..),
  E_Write(..), E_Read(..),
  Block(..), EPIPE(..), NotReadable(..), NotWritable(..), PipeEnds(..),
  NoSuchProcess(..),
  ) where

import Control.Monad (ap,liftM)
import Environment (Environment)
import Interaction (Prompt,OutMode,EOF)
import Path (Path)

newtype Pid = Pid Int deriving (Eq,Ord,Num)
instance Show Pid where show (Pid n) = "[" ++ show n ++ "]"

data Command = Command { argv :: (String,[String]) }
instance Show Command where show (Command (x,xs)) = unwords (x:xs)

newtype FD = FD Int
  deriving (Eq,Ord,Enum,Num)

instance Show FD where show (FD n) = "&" ++ show n

data BadFileDescriptor = BadFileDescriptor deriving Show

data NoSuchProcess = NoSuchProcess

data Prog a where
  Ret :: a -> Prog a
  Bind :: Prog a -> (a -> Prog b) -> Prog b
  Exit :: Prog a
  WriteConsole :: OutMode -> String -> Prog ()
  Trace :: String -> Prog ()
  Fork :: Prog (Maybe Pid)
  Exec :: Environment -> Command -> Prog a -> Prog b
  Kill :: Pid -> Prog (Either NoSuchProcess ())
  Wait :: Pid -> Prog ()
  Alive :: Pid -> Prog Bool
  Argv :: Prog Command
  MyPid :: Prog Pid
  MyEnvironment :: Prog Environment
  Procs :: Prog [(Pid,Command)]
  Lsof :: Prog [(Pid,Command,FD,OF)]
  Call :: (Show a, Show b) => SysCall a b -> a -> Prog b

instance Show (Prog a) where show _ = "<PROG>"

instance Functor Prog where fmap = liftM
instance Applicative Prog where pure = return; (<*>) = ap
instance Monad Prog where return = Ret; (>>=) = Bind

data SysCall a b where
  LoadBinary :: SysCall Path (Either LoadBinaryError (Prog ()))
  Kind :: SysCall Path (Either NoSuchPath FileKind)
  Open :: SysCall (Path,OpenMode) (Either OpenError FD)
  Close :: SysCall FD (Either BadFileDescriptor ())
  Dup2 :: SysCall (FD,FD) (Either BadFileDescriptor ())
  Read :: Prompt -> SysCall FD (Either E_Read (Either EOF String))
  Write :: SysCall (FD,String) (Either E_Write ())
  Paths :: SysCall () [Path]
  Mv :: SysCall (Path,Path) (Either NoSuchPath ())
  Rm :: SysCall Path (Either NoSuchPath ())
  SysPipe :: SysCall () (PipeEnds FD)
  Fds :: SysCall () [FD]

data E_Read
  = ER_BadFileDescriptor
  | ER_NotReadable
  deriving Show

data E_Write
  = EW_BadFileDescriptor
  | EW_NotWritable
  | EW_PIPE
  deriving Show

data NoSuchPath = NoSuchPath
  deriving Show

data BinaryMeta = BinaryMeta String
  deriving Show

data FileKind = K_Data | K_Binary BinaryMeta
  deriving Show

data OpenMode
  = OpenForReading -- creating if doesn't exist
  | OpenForWriting WriteOpenMode -- rm, then append
  deriving Show

data WriteOpenMode = Truncate | Append deriving Show

data OpenError
  = OE_NoSuchPath
  | OE_CantOpenForReading
  deriving Show

data LoadBinaryError
  = LBE_NoSuchPath
  | LBE_CantLoadAsBinary
  deriving Show

instance Show (SysCall a b) where -- TODO: automate?
  show = \case
    Kind -> "Kind"
    LoadBinary -> "LoadBinary"
    Open -> "Open"
    Close -> "Close"
    Dup2 -> "Dup2"
    Read _ -> "Read"
    Write -> "Write"
    Paths -> "Paths"
    Mv -> "Mv"
    Rm -> "Rm"
    SysPipe -> "Pipe"
    Fds -> "Fds"

newtype PipeKey = PipeKey Int deriving (Eq,Ord,Num)
instance Show PipeKey where show (PipeKey n) = "pipe"++show n

data OF -- opened file
  = PipeRead PipeKey
  | PipeWrite PipeKey
  | FileAppend Path -- nothing done when opened
  | FileContents [String] -- full contents read when opened

instance Show OF where
  show = \case
    PipeRead pk -> "Read:"++show pk
    PipeWrite pk -> "Write:"++show pk
    FileAppend path -> "Append:"++show path
    FileContents xs -> "Contents[size=#"++show (length xs)++"]"


data Block = Block deriving Show
data EPIPE = EPIPE deriving Show
data NotReadable = NotReadable deriving Show
data NotWritable = NotWritable deriving Show
data PipeEnds a = PipeEnds { r :: a, w :: a } deriving Show
