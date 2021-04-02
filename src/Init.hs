-- | 'init' is the top-level program which runs on MeNicks
module Init (init) where

import Lib (forkWait,tryLoadBinary,write,stderr,exit,execSameEnv)
import Prelude hiding (init,read)
import Prog (Prog(..),Command(..))

init :: Prog ()
init = do
  forkWait $ do
    tryLoadBinary "sham" >>= \case
      Nothing -> do write stderr "init : cannot find sham interpreter"; exit
      Just prog -> do
        execSameEnv (Command ("sham",[])) prog
