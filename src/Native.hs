
module Native (
  ls,ps,rev,cat,echo,xargs,builtins,
  withOpen, readAll, read, write, err2,
  ) where

import Data.List (sort,sortOn)
import Interaction (Prompt(..))
import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Prog (Prog,OpenMode(..),NoSuchPath(..))
import Path (Path)
import Prelude hiding (all,read)
import SysCall (SysCall(..),FD)
import qualified Prog
import qualified Path (create,toString)

echo :: [String] -> Prog ()
echo args = write stdout (unwords args)

cat :: [String] -> Prog ()
cat = \case
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

rev :: [String] -> Prog ()
rev args = checkNoArgs "rev" args loop where
  loop :: Prog ()
  loop = do
    read NoPrompt stdin >>= \case
      Left EOF -> pure ()
      Right line -> do
        write stdout (reverse line)
        loop

ls :: [String] -> Prog ()
ls args = checkNoArgs "ls" args $ do
  paths <- Prog.Call Paths ()
  mapM_ (write stdout . Path.toString) (sort paths)

ps :: [String] -> Prog ()
ps args = checkNoArgs "ps" args $ do
  xs <- Prog.Procs
  sequence_
    [ write stdout (show pid ++ " " ++ show p)  | (pid,p) <- sortOn fst xs ]

builtins :: [String] -> [String] -> Prog ()
builtins names args = checkNoArgs "builtins" args $ do
  mapM_ (write stdout) $ names

xargs :: (String -> [String] -> Prog ()) -> [String] -> Prog ()
xargs runCom = \case
  [] -> err2 "xargs: needs at least 1 argument"
  x:args -> do
    lines <- readAll stdin
    runCom x (args ++ lines)

-- TODO: checkNoArgs should get who and args from Prog env
checkNoArgs :: String -> [String] -> Prog () -> Prog ()
checkNoArgs who args prog = case args of
  [] -> prog
  _ -> err2 (who ++ ": takes no arguments")

withOpen :: Path -> OpenMode -> (FD -> Prog a) -> Prog a
withOpen path mode action =
  Prog.Call Open (path,mode) >>= \case
    Left NoSuchPath -> do
      err2 $ "no such path: " ++ Path.toString path
      Prog.Exit
    Right fd -> do
      res <- action fd
      Prog.Call Close fd
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
  Prog.Call (Read prompt) fd >>= \case
    Left NotReadable -> do
      err2 (show fd ++ " not readable")
      pure (Left EOF) -- TODO: better to exit?
    Right eofOrLine -> do
      pure eofOrLine

write :: FD -> String -> Prog ()
write fd line = do
  Prog.Call Write (fd,line) >>= \case
    Left NotWritable -> err2 (show fd ++ " not writable")
    Right (Left EPIPE) -> err2 "EPIPE when writing to fd1"
    Right (Right ()) -> pure ()

err2 :: String -> Prog ()
err2 line = do
  Prog.Call Write (stderr, line) >>= \case
    Left NotWritable -> Prog.Trace (show stderr ++ " not writable")
    Right (Left EPIPE) -> Prog.Trace "EPIPE when writing to fd2"
    Right (Right ()) -> pure ()

stdin,stdout,stderr :: FD
stdin = 0
stdout = 1
stderr = 2
