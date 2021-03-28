-- | 'init' is the top-level program which runs on MeNicks
module Init (init) where

import Interaction (Prompt(..),OutMode(..))
import Lib (tryLoadBinary,write,stdout,stderr,exit,execSameEnv,dup2)
import Misc (PipeEnds(..),EOF(..))
import Prelude hiding (init)
import Prog (Prog(..),Pid,FD,SysCall(..),Command(..))
import qualified Lib (read)

init :: Prog ()
init = tryLoadBinary "sham" >>= \case
  Nothing -> do write stderr "init : cannot find sham interpreter"; exit
  Just prog -> do
    PipeEnds{r=rOut,w=wOut} <- Call SysPipe ()
    Prog.Fork >>= \case
      Just pidShell -> do
        Call Close wOut
        execSameEnv (Command ("tty-monitor-stdout",[])) (monitor pidShell rOut)
      Nothing -> do
        Call Close rOut
        dup2 stdout wOut
        Call Close wOut
        execSameEnv (Command ("sham",[])) prog

monitor :: Pid -> FD -> Prog ()
monitor shell fd = loop 0 where
  loop :: Int -> Prog ()
  loop i = do
    Alive shell >>= \case
      False -> pure ()
      True -> do
        Lib.read NoPrompt fd >>= \case
          Left EOF -> pure ()
          Right line -> do WriteConsole Normal line
        loop (i+1)
