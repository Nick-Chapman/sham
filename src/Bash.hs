
module Bash (console) where

import Prelude hiding (read)
import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Os (FD(..),Prog(..))

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
  xs -> Echo2 ("bash, unable to parse: " ++ show xs)

data Script
  = Echo String
  | Echo2 String
  | ExecRev
  | Null

interpret :: Script -> Prog ()
interpret = \case
  Echo line -> write (FD 1) line
  Echo2 line -> write (FD 2) line
  ExecRev -> revProg
  Null  -> pure ()

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
