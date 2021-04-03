-- | State of the 'MeNicks' operating system.
module Kernel (State(..), Proc(..), FdEnv(..), Action(..)) where

import Data.List (intercalate)
import Data.Map (Map)
import Environment (Environment)
import Interaction (OutMode)
import OpenFileTable (OpenFileTable,Key)
import Prog (Pid,Command,FD,NoSuchProcess,OF,SysCall)
import qualified Data.Map.Strict as Map

data State = State -- system wide state
  { os :: OpenFileTable
  , nextPid :: Pid
  , waiting :: Map Pid Proc
  , suspended :: Map Pid Proc
  }

instance Show State where
  show Kernel.State{os} = show os

data Proc = Proc -- per process state
  { command :: Command
  , environment :: Environment
  , fde :: FdEnv
  , action :: Action
  }

newtype FdEnv = FdEnv { unFdEnv :: Map FD OpenFileTable.Key }

instance Show FdEnv where
  show FdEnv{unFdEnv=m} =
    intercalate ", " [ show k ++ "=" ++ show e | (k,e) <- Map.toList m ]

data Action where
  A_Halt :: Action
  A_WriteConsole :: OutMode -> String -> Action -> Action
  A_Trace :: String -> Action -> Action
  A_Fork :: (Maybe Pid -> Action) -> Action
  A_Exec :: Environment -> Command -> Action -> Action
  A_Wait :: Pid -> Action -> Action
  A_Kill :: Pid -> (Either NoSuchProcess () -> Action) -> Action
  A_Alive :: Pid -> (Bool -> Action) -> Action
  A_Argv :: (Command -> Action) -> Action
  A_MyPid :: (Pid -> Action) -> Action
  A_MyEnvironment :: (Environment -> Action) -> Action
  A_Procs :: ([(Pid,Command)] -> Action) -> Action
  A_Lsof :: ([(Pid,Command,FD,OF)] -> Action) -> Action
  A_Call :: (Show a, Show b) => SysCall a b -> a -> (b -> Action) -> Action
