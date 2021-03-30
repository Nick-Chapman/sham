-- | A 'library' of program fragments. Used by Bins, and Sham/Script.
module Lib (
  stdin, stdout, stderr, shift2, closeAllBut,
  close, read, write, dup2, exit, forkWait, forkNoWait, tryLoadBinary, execCommand,
  checkNoArgs, checkAtLeastOneArg, getSingleArg, getTwoArgs,
  loadFile, withOpen, readAll, execSameEnv,
  ) where

import Environment (Environment)
import Interaction (Prompt(..),EOF(..))
import Path (Path)
import Prelude hiding (head,read,sum)
import Prog
import qualified Path (create,toString)

stdin,stdout,stderr :: FD
stdin = 0
stdout = 1
stderr = 2

-- | Shift open file from src to dest file-descriptor, by calling dup2 and close
-- | take special case when src & dest are the same!
shift2 :: FD -> FD -> Prog ()
shift2 dest src = do
  if (src == dest) then pure () else do
    dup2 dest src; close src

closeAllBut :: [FD] -> Prog ()
closeAllBut preserve = do
  open <- Call Fds ()
  sequence_ [ close fd | fd <- open, not (fd `elem` preserve) ]

read :: Prompt -> FD -> Prog (Either EOF String)
read prompt fd =
  Call (Read prompt) fd >>= \case
    Left ER_BadFileDescriptor -> errFD fd
    Left ER_NotReadable -> err2 (show fd ++ " not readable")
    Right eofOrLine -> pure eofOrLine

write :: FD -> String -> Prog ()
write fd line = do
  Call Write (fd,line) >>= \case
    Left EW_BadFileDescriptor -> errFD fd
    Left EW_NotWritable -> err2 (show fd ++ " not writable")
    Left EW_PIPE -> exit
    Right () -> pure ()

close :: FD -> Prog ()
close fd = do
  Call Close fd >>= \case
    Left BadFileDescriptor -> errFD fd
    Right () -> pure ()

dup2 :: FD -> FD -> Prog ()
dup2 d s = do
  Call Dup2 (d,s) >>= \case
    Left BadFileDescriptor -> errFD s
    Right () -> pure ()

errFD :: FD -> Prog a
errFD fd = do
  err2 ("bad file descriptor: " ++ show fd)

err2 :: String -> Prog a
err2 line = do
  Call Write (stderr, line) >>= \case
    Left EW_NotWritable -> Trace (show stderr ++ " not writable")
    Left EW_BadFileDescriptor -> Trace (show stderr ++ " bad file desc")
    Left EW_PIPE -> Trace "EPIPE when writing to fd2"
    Right () -> pure ()
  exit

exit :: Prog a
exit = Exit

forkWait :: Prog () -> Prog ()
forkWait prog = do
  Fork >>= \case
    Nothing -> prog
    Just pid -> Wait pid

forkNoWait :: Prog () -> Prog ()
forkNoWait prog = do
  Fork >>= \case
    Nothing -> prog
    Just _ -> pure ()

tryLoadBinary :: String -> Prog (Maybe (Prog ()))
tryLoadBinary name = do
  Call LoadBinary (Path.create name) >>= \case
    Right prog -> do
      pure (Just prog)
    Left LBE_CantLoadAsBinary -> do
      pure Nothing
    Left LBE_NoSuchPath -> do
      err2 $ "no such executable: " ++ name

execCommand :: Environment -> Command -> Prog ()
execCommand environment command@(Command (name,args))  = do
  --Trace (show ("execCommand",command))
  tryLoadBinary name >>= \case
    Just prog -> do
      Exec environment command prog
    Nothing -> do
      let com = "sham"
      tryLoadBinary com >>= \case
        Just prog -> do
          Exec environment (Command (com,name:args)) prog
        Nothing -> do
          err2 "cant find sham interpreter"

checkNoArgs :: Prog () -> Prog ()
checkNoArgs prog = do
  Command(com,args) <- Argv
  case args of
    [] -> prog
    _ -> write stderr (com ++ ": takes no arguments")

checkAtLeastOneArg :: Prog () -> Prog ()
checkAtLeastOneArg prog = do
  Command(com,args) <- Argv
  case args of
    [] -> write stderr (com ++ ": takes at least one argument")
    _ -> prog

getSingleArg :: (String -> Prog ()) -> Prog ()
getSingleArg f = do
  Command(com,args) <- Argv
  case args of
    [arg] -> f arg
    _ -> write stderr (com ++ ": takes a single argument")

getTwoArgs :: (String -> String -> Prog ()) -> Prog ()
getTwoArgs f = do
  Command(com,args) <- Argv
  case args of
    [arg1,arg2] -> f arg1 arg2
    _ -> write stderr (com ++ ": takes two arguments")

loadFile :: String -> Prog [String]
loadFile path = do
  withOpen (Path.create path) OpenForReading $ \fd -> do
    readAll fd

withOpen :: Path -> OpenMode -> (FD -> Prog a) -> Prog a
withOpen path mode action =
  Call Open (path,mode) >>= \case
    Left OE_NoSuchPath -> do
      write stderr $ "no such path: " ++ Path.toString path
      exit
    Left OE_CantOpenForReading -> do
      write stderr $ "cant open for reading: " ++ Path.toString path
      exit
    Right fd -> do
      res <- action fd
      close fd
      pure res

readAll :: FD -> Prog [String]
readAll fd = loop []
  where
    loop acc =
      read NoPrompt fd >>= \case
      Left EOF -> pure (reverse acc)
      Right line -> loop (line:acc)

execSameEnv :: Command -> Prog () -> Prog ()
execSameEnv command prog = do
  e <- MyEnvironment
  Exec e command prog
