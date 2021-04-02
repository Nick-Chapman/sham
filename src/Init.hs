-- | 'init' is the top-level program which runs on MeNicks
module Init (init) where

import Interaction (Prompt(..),OutMode(..),EOF(..))
import Lib (tryLoadBinary,forkWait,write,stdout,stderr,exit,execSameEnv,shift2,closeAllBut,close,read)
import Prelude hiding (init,read)
import Prog (Prog(..),FD,SysCall(..),Command(..),PipeEnds(..))

init :: Prog ()
init = do
  forkWait $ do -- not essential; allows "init" process to remain
    let _ = monitor (100+stderr) StdErr -- TODO: step to removing. put on fd 102
    let _ = monitor (100+stdout) StdOut -- TODO: step to removing. put on fd 101
    tryLoadBinary "sham" >>= \case
      Nothing -> do write stderr "init : cannot find sham interpreter"; exit
      Just prog -> do
        execSameEnv (Command ("sham",[])) prog

monitor :: FD -> OutMode -> Prog ()
monitor dest mode = do
    PipeEnds{r,w} <- Call SysPipe ()
    Prog.Fork >>= \case
      Nothing -> do
        -- continue in the child!
        close r
        shift2 dest w
      Just pid -> do
        closeAllBut [r]
        let
          loop :: Prog ()
          loop = do
            Alive pid >>= \case
              False -> pure ()
              True -> do
                read NoPrompt r >>= \case
                  Left EOF -> pure ()
                  Right line -> do
                    WriteConsole mode line
                    loop
        let com = "tty-"++show mode
        execSameEnv (Command (com,[])) loop
