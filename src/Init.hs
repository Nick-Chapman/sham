-- | 'init' is the top-level program which runs on MeNicks
module Init (init) where

import Interaction (Prompt(..),OutMode(..))
import Lib (tryLoadBinary,write,stdout,stderr,exit,execSameEnv,dup2,close,read)
import Misc (PipeEnds(..),EOF(..))
import Prelude hiding (init,read)
import Prog (Prog(..),Pid,FD,SysCall(..),Command(..))

init :: Prog ()
init = tryLoadBinary "sham" >>= \case
  Nothing -> do write stderr "init : cannot find sham interpreter"; exit
  Just prog -> do
    PipeEnds{r=rErr,w=wErr} <- Call SysPipe ()
    PipeEnds{r=rOut,w=wOut} <- Call SysPipe ()
    Prog.Fork >>= \case
      Just pidShell -> do
        close wOut
        close wErr
        Prog.Fork >>= \case
          Just _ -> do
            close rErr
            execSameEnv (Command ("tty-monitor-stdout",[])) (monitor Normal pidShell rOut)
          Nothing -> do
            close rOut
            execSameEnv (Command ("tty-monitor-stderr",[])) (monitor StdErr pidShell rErr)

      Nothing -> do
        close rOut; shift2 stdout wOut
        close rErr; shift2 stderr wErr
        execSameEnv (Command ("sham",[])) prog


-- | Shift open file from src to dest file-descriptor, by calling dup2 and close
-- | take special case when src & dest are the same!
shift2 :: FD -> FD -> Prog ()
shift2 dest src = do
  --if (src == dest) then pure () else do -- TODO: instate this, make things work!
    dup2 dest src; close src

monitor :: OutMode -> Pid -> FD -> Prog ()
monitor mode shell fd = loop 0 where
  loop :: Int -> Prog ()
  loop i = do
    Alive shell >>= \case
      False -> pure ()
      True -> do
        read NoPrompt fd >>= \case
          Left EOF -> pure ()
          Right line -> do WriteConsole mode line
        loop (i+1)
