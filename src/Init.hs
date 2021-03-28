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
    PipeEnds{r=rErr,w=wErr} <- Call SysPipe ()
    Prog.Fork >>= \case
      Just pidShell -> do
        Call Close wOut
        Call Close wErr
        Prog.Fork >>= \case
          Just _ -> do
            Call Close rErr
            execSameEnv (Command ("tty-monitor-stdout",[])) (monitor Normal pidShell rOut)
          Nothing -> do
            Call Close rOut
            execSameEnv (Command ("tty-monitor-stderr",[])) (monitor StdErr pidShell rErr)

      Nothing -> do
        Call Close rOut; dup2 stdout wOut; Call Close wOut
        Call Close rErr; dup2 stderr wErr; Call Close wErr
        execSameEnv (Command ("sham",[])) prog


monitor :: OutMode -> Pid -> FD -> Prog ()
monitor mode shell fd = loop 0 where
  loop :: Int -> Prog ()
  loop i = do
    Alive shell >>= \case
      False -> pure ()
      True -> do
        Lib.read NoPrompt fd >>= \case
          Left EOF -> pure ()
          Right line -> do WriteConsole mode line
        loop (i+1)
