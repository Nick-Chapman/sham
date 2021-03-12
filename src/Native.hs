module Native (
  echo, cat, rev, grep, head, ls, ps, bins, xargs, ifeq, man,
  withOpen, readAll, read, write, err2, checkNoArgs
  ) where

import Control.Monad (when)
import Data.List (sort,sortOn,isInfixOf)
import Data.Map (Map)
import Interaction (Prompt(..))
import MeNicks (Prog,Command(..),OpenMode(..),NoSuchPath(..))
import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Path (Path)
import Prelude hiding (all,head,read)
import SysCall (SysCall(..),FD)
import qualified Data.Map.Strict as Map
import qualified MeNicks (Prog(..))
import qualified Path (create,toString)

echo :: Prog ()
echo = do
  Command(_,args) <- MeNicks.Argv
  write stdout (unwords args)

cat :: Prog ()
cat = do
  Command(_,args) <- MeNicks.Argv
  case args of
    [] -> catFd stdin
    args -> sequence_ [ catProg1 (Path.create arg) | arg <- args ]
  where
    catProg1 :: Path -> Prog ()
    catProg1 path = withOpen path OpenForReading $ catFd

    catFd :: FD -> Prog ()
    catFd fd = loop where
      loop :: Prog ()
      loop = do
        read NoPrompt fd >>= \case
          Left EOF -> pure ()
          Right line -> do
            write stdout line
            loop

rev :: Prog ()
rev = checkNoArgs loop where
  loop :: Prog ()
  loop = do
    read NoPrompt stdin >>= \case
      Left EOF -> pure ()
      Right line -> do
        write stdout (reverse line)
        loop

grep :: Prog ()
grep = do
  getSingleArg $ \pat -> do
  let
    loop :: Prog ()
    loop = do
      read NoPrompt stdin >>= \case
        Left EOF -> pure ()
        Right line -> do
          when (pat `isInfixOf` line) $
            write stdout line
          loop
  loop

head :: Prog ()
head = checkNoArgs $ do
  Native.read NoPrompt stdin >>= \case
    Left EOF -> pure ()
    Right line -> write stdout line

ls :: Prog ()
ls = checkNoArgs $ do
  paths <- MeNicks.Call Paths ()
  mapM_ (write stdout . Path.toString) (sort paths)

ps :: Prog ()
ps = checkNoArgs $ do
  xs <- MeNicks.Procs
  sequence_
    [ write stdout (show pid ++ " " ++ show com)  | (pid,com) <- sortOn fst xs ]

bins :: [String] -> Prog ()
bins names = checkNoArgs $ do
  mapM_ (write stdout) $ names

xargs :: (Command -> Prog ()) -> Prog ()
xargs runCommand = do
  Command(me,args) <- MeNicks.Argv
  case args of
    [] -> err2 (me ++ ": takes at least 1 argument")
    com:args -> do
      lines <- readAll stdin
      runCommand (Command (com, args ++ lines))

ifeq :: (Command -> Prog ()) -> Prog ()
ifeq runCommand = do
  Command(me,args) <- MeNicks.Argv
  case args of
    a:b:com:args -> do
      if a == b then runCommand (Command (com, args)) else pure ()
    _ ->
      err2 (me ++ ": takes at least 3 arguments")

man :: Map String String -> Prog ()
man docsMap  = do
  Command(_,args) <- MeNicks.Argv
  mapM_ manline args
  where
    manline :: String -> Prog ()
    manline name =
      case Map.lookup name docsMap of
        Just text -> write stdout (name ++ " : " ++ text)
        Nothing -> write stderr (name ++ " : no manual entry")

checkNoArgs :: Prog () -> Prog ()
checkNoArgs prog = do
  Command(com,args) <- MeNicks.Argv
  case args of
    [] -> prog
    _ -> err2 (com ++ ": takes no arguments")

getSingleArg :: (String -> Prog ()) -> Prog ()
getSingleArg f = do
  Command(com,args) <- MeNicks.Argv
  case args of
    [arg] -> f arg
    _ -> err2 (com ++ ": takes a single argument")

withOpen :: Path -> OpenMode -> (FD -> Prog a) -> Prog a
withOpen path mode action =
  MeNicks.Call Open (path,mode) >>= \case
    Left NoSuchPath -> do
      err2 $ "no such path: " ++ Path.toString path
      MeNicks.Exit
    Right fd -> do
      res <- action fd
      MeNicks.Call Close fd
      pure res

readAll :: FD -> Prog [String]
readAll fd = loop []
  where
    loop acc =
      Native.read NoPrompt fd >>= \case
      Left EOF -> pure (reverse acc)
      Right line -> loop (line:acc)

read :: Prompt -> FD -> Prog (Either EOF String)
read prompt fd =
  MeNicks.Call (Read prompt) fd >>= \case
    Left NotReadable -> do err2 (show fd ++ " not readable"); MeNicks.Exit
    Right eofOrLine -> pure eofOrLine

write :: FD -> String -> Prog ()
write fd line = do
  MeNicks.Call Write (fd,line) >>= \case
    Left NotWritable -> do err2 (show fd ++ " not writable"); MeNicks.Exit
    Right (Left EPIPE) -> MeNicks.Exit
    Right (Right ()) -> pure ()

err2 :: String -> Prog ()
err2 line = do
  MeNicks.Call Write (stderr, line) >>= \case
    Left NotWritable -> MeNicks.Trace (show stderr ++ " not writable")
    Right (Left EPIPE) -> MeNicks.Trace "EPIPE when writing to fd2"
    Right (Right ()) -> pure ()

stdin,stdout,stderr :: FD
stdin = 0
stdout = 1
stderr = 2
