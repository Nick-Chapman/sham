
module Bash (console) where

import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Os (FD(..),Prog(..),OpenMode(..),NoSuchPath(..))
import Path (Path)
import Prelude hiding (read)
import qualified Path (create,toString)

console :: Prog ()
console = loop where
  loop :: Prog ()
  loop = do
    write (FD 1) "prompt $"
    read (FD 0) >>= \case
      Left EOF -> pure ()
      Right line -> do
        interpret (parseLine line)
        loop

parseLine :: String -> Script
parseLine s = case words s of
  [] -> Null
  ["echo",s] -> Echo s
  ["rev"] -> ExecRev
  ["ls"] -> ExecLs
  ["cat",s] -> ExecCat (Path.create s)
  [".",s] -> Source (Path.create s)
  xs -> Echo2 ("bash, unable to parse: " ++ show xs)

data Script
  = Null
  | Echo String
  | Echo2 String
  | ExecRev
  | ExecLs
  | ExecCat Path
  | Source Path

interpret :: Script -> Prog ()
interpret = \case
  Null  -> pure ()
  Echo line -> write (FD 1) line
  Echo2 line -> write (FD 2) line
  ExecRev -> revProg
  ExecLs -> lsProg
  ExecCat path -> catProg path
  Source path -> sourceProg path


sourceProg :: Path -> Prog ()
sourceProg path = do
  Open path OpenForReading >>= \case
    Left NoSuchPath -> err2 $ "source, no such path: " ++ Path.toString path
    Right fd -> do
      let
        loop :: Prog ()
        loop = do
          read fd >>= \case
            Left EOF -> pure ()
            Right line -> do
              let script = parseLine line
              interpret script
              loop
      loop
      -- TODO: close fd


catProg :: Path -> Prog ()
catProg path = do
  Open path OpenForReading >>= \case
    Left NoSuchPath -> err2 $ "cat, no such path: " ++ Path.toString path
    Right fd -> do
      let
        loop :: Prog ()
        loop = do
          read fd >>= \case
            Left EOF -> pure ()
            Right line -> do
              write (FD 1) line
              loop
      loop
      -- TODO: close fd

lsProg :: Prog ()
lsProg = do
  paths <- Ls
  mapM_ (write (FD 1) . Path.toString) paths

revProg :: Prog ()
revProg = loop where
  loop :: Prog ()
  loop = do
    read (FD 0) >>= \case
      Left EOF -> pure ()
      Right line -> do
        write (FD 1) (reverse line)
        loop

read :: FD -> Prog (Either EOF String)
read fd =
  Read fd >>= \case
    Left NotReadable -> do
      err2 "echo, fd0 not readable"
      pure (Left EOF) -- TODO: better to exit?
    Right eofOrLine -> do
      pure eofOrLine

write :: FD -> String -> Prog ()
write fd mes = do
  Write fd mes >>= \case
    Left NotWritable -> err2 "echo, fd1 not writable"
    Right (Left EPIPE) -> err2 "echo, EPIPE when writing to fd1"
    Right (Right ()) -> pure ()

err2 :: String -> Prog ()
err2 mes = do
  Write (FD 2) mes >>= \case
    Left NotWritable -> Trace "echo, fd2 not writable"
    Right (Left EPIPE) -> Trace "echo, EPIPE when writing to fd2"
    Right (Right ()) -> pure ()
