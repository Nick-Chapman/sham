module Bash (Script(..),interpret) where

import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Os (FD(..),Prog(..))

data Script
  = Echo String
  | ExecRev

interpret :: Script -> Prog ()
interpret = \case
  Echo line -> write1 line
  ExecRev ->
    revProg

revProg :: Prog ()
revProg = loop where
  loop :: Prog ()
  loop = do
    read0 >>= \case
      Left EOF -> pure ()
      Right line -> do
        write1 (reverse line)
        loop

read0 :: Prog (Either EOF String)
read0 =
  Read (FD 0) >>= \case
    Left NotReadable -> do
      err2 "echo, fd0 not readable"
      pure (Left EOF) -- TODO: better to exit?
    Right eofOrLine -> do
      pure eofOrLine

write1 :: String -> Prog ()
write1 mes = do
  Write (FD 1) mes >>= \case
    Left NotWritable -> err2 "echo, fd1 not writable"
    Right (Left EPIPE) -> err2 "echo, EPIPE when writing to fd1"
    Right (Right ()) -> pure ()

err2 :: String -> Prog ()
err2 mes = do
  Write (FD 2) mes >>= \case
    Left NotWritable -> Trace "echo, fd2 not writable"
    Right (Left EPIPE) -> Trace "echo, EPIPE when writing to fd2"
    Right (Right ()) -> pure ()
