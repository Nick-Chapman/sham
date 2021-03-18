-- | A 'library' of program fragments. Used by Bins, and Sham/Script.
module Lib (
  stdin, stdout, stderr,
  read, write, exit,
  checkNoArgs, getSingleArg, loadFile, withOpen, readAll,
  ) where

import Interaction (Prompt(..))
import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Path (Path)
import Prelude hiding (head,read,sum)
import Prog (Prog,Command(..),OpenMode(..),SysCall(..),FD,OpenError(..))
import qualified Path (create,toString)
import qualified Prog (Prog(..))

stdin,stdout,stderr :: FD
stdin = 0
stdout = 1
stderr = 2

read :: Prompt -> FD -> Prog (Either EOF String)
read prompt fd =
  Prog.Call (Read prompt) fd >>= \case
    Left NotReadable -> do err2 (show fd ++ " not readable"); exit
    Right eofOrLine -> pure eofOrLine

write :: FD -> String -> Prog ()
write fd line = do
  Prog.Call Write (fd,line) >>= \case
    Left NotWritable -> do err2 (show fd ++ " not writable"); exit
    Right (Left EPIPE) -> exit
    Right (Right ()) -> pure ()

err2 :: String -> Prog ()
err2 line = do
  Prog.Call Write (stderr, line) >>= \case
    Left NotWritable -> Prog.Trace (show stderr ++ " not writable")
    Right (Left EPIPE) -> Prog.Trace "EPIPE when writing to fd2"
    Right (Right ()) -> pure ()

exit :: Prog a
exit = Prog.Exit

checkNoArgs :: Prog () -> Prog ()
checkNoArgs prog = do
  Command(com,args) <- Prog.Argv
  case args of
    [] -> prog
    _ -> write stderr (com ++ ": takes no arguments")

getSingleArg :: (String -> Prog ()) -> Prog ()
getSingleArg f = do
  Command(com,args) <- Prog.Argv
  case args of
    [arg] -> f arg
    _ -> write stderr (com ++ ": takes a single argument")

loadFile :: String -> Prog [String]
loadFile path = do
  withOpen (Path.create path) OpenForReading $ \fd -> do
    readAll fd

withOpen :: Path -> OpenMode -> (FD -> Prog a) -> Prog a
withOpen path mode action =
  Prog.Call Open (path,mode) >>= \case
    Left OE_NoSuchPath -> do
      write stderr $ "no such path: " ++ Path.toString path
      exit
    Left OE_CantOpenForReading -> do
      write stderr $ "cant open for reading: " ++ Path.toString path
      exit
    Right fd -> do
      res <- action fd
      Prog.Call Close fd
      pure res

readAll :: FD -> Prog [String]
readAll fd = loop []
  where
    loop acc =
      read NoPrompt fd >>= \case
      Left EOF -> pure (reverse acc)
      Right line -> loop (line:acc)
