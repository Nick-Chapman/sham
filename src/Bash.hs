
module Bash (console) where

import Data.List (sort)
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
parseLine line = case words line of -- TODO: at some point I'll want a less hacky parser
  [] -> Null
  ["echo",s] -> Echo s
  ["echo",s1,s2] -> Echo (unwords [s1,s2])
  ["echo",s,">>",p] -> EchoR s (Redirect OpenForAppending (FD 1) (FromPath (Path.create p)))
  ["echo",s1,s2,">>",p] -> EchoR (unwords [s1,s2]) (Redirect OpenForAppending (FD 1) (FromPath (Path.create p)))
  ["rev"] -> ExecRev
  ["rev", "<", p] -> ExecRevR (Redirect OpenForReading (FD 0) (FromPath (Path.create p)))
  ["rev", ">>", p] -> ExecRevR (Redirect OpenForAppending (FD 1) (FromPath (Path.create p)))

  ["rev","<",p1,">>",p2] ->
    ExecRevRs [ Redirect OpenForReading (FD 0) (FromPath (Path.create p1))
              , Redirect OpenForAppending (FD 1) (FromPath (Path.create p2)) ]

  ["ls"] -> ExecLs
  ["cat",s] -> ExecCat (Path.create s)
  [".",s] -> Source (Path.create s)
  [s] -> ExecWait (Path.create s)
  [s, "<", p] -> ExecWaitR (Path.create s) (Redirect OpenForReading (FD 0) (FromPath (Path.create p)))
  [s, ">>", p] -> ExecWaitR (Path.create s) (Redirect OpenForAppending (FD 1) (FromPath (Path.create p)))
  xs ->
    Echo2 ("bash, unable to parse: " ++ show xs)

data Script
  = Null
  | Echo String
  | Echo2 String
  | ExecRev
  | ExecRevR Redirect
  | ExecRevRs [Redirect]
  | ExecLs
  | ExecCat Path
  | Source Path
  | EchoR String Redirect
  | ExecWait Path
  | ExecWaitR Path Redirect

data Redirect
  = Redirect OpenMode FD RedirectSource

data RedirectSource
  = FromPath Path
--  | FromFD FD

interpret :: Script -> Prog ()
interpret = \case
  Null  -> pure ()
  Echo line -> echoProg line
  Echo2 line -> write (FD 2) line -- TODO: use redirect
  ExecRev -> revProg
  ExecRevR r -> applyRedirect r revProg
  ExecRevRs rs -> applyRedirects rs revProg
  ExecLs -> lsProg
  ExecCat path -> catProg path
  Source path -> sourceProg path
  EchoR line r -> applyRedirect r (echoProg line)
  ExecWait path -> execWait path
  ExecWaitR path r -> applyRedirect r (execWait path)


applyRedirects :: [Redirect] -> Prog () -> Prog ()
applyRedirects = \case
  [] -> \prog -> prog
  r:rs -> applyRedirect r . applyRedirects rs

applyRedirect :: Redirect -> Prog () -> Prog ()
applyRedirect r prog =
  case r of
    Redirect mode dFd (FromPath path) -> do
      withOpen path mode $ \sFd -> do
        SavingEnv $ do
          Dup2 dFd sFd
          prog

echoProg :: String -> Prog ()
echoProg line = write (FD 1) line

execWait :: Path -> Prog ()
execWait path = do
  withOpen path OpenForReading $ \fd -> do
    lines <- readAll fd
    sequence_ [ interpret (parseLine line) | line <- lines ]

readAll :: FD -> Prog [String]
readAll fd = loop []
  where
    loop acc =
      read fd >>= \case
      Left EOF -> pure (reverse acc)
      Right line -> loop (line:acc)

sourceProg :: Path -> Prog ()
sourceProg path = do
  withOpen path OpenForReading $ \fd -> do
    let
      loop :: Prog ()
      loop = do
        -- interleaves read and interpretation
        read fd >>= \case
          Left EOF -> pure ()
          Right line -> do
            let script = parseLine line
            interpret script
            loop
    loop

catProg :: Path -> Prog ()
catProg path = do
  withOpen path OpenForReading $ \fd -> do
    let
      loop :: Prog ()
      loop = do
        read fd >>= \case
          Left EOF -> pure ()
          Right line -> do
            write (FD 1) line
            loop
    loop

withOpen :: Path -> OpenMode -> (FD -> Prog ()) -> Prog ()
withOpen path mode action =
  Open path mode >>= \case
    Left NoSuchPath -> err2 $ "no such path: " ++ Path.toString path
    Right fd -> do
      action fd
      Close fd

lsProg :: Prog ()
lsProg = do
  paths <- Ls
  mapM_ (write (FD 1) . Path.toString) (sort paths)

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
      err2 "fd0 not readable"
      pure (Left EOF) -- TODO: better to exit?
    Right eofOrLine -> do
      pure eofOrLine

write :: FD -> String -> Prog ()
write fd line = do
  Write fd line >>= \case
    Left NotWritable -> err2 "fd1 not writable"
    Right (Left EPIPE) -> err2 "EPIPE when writing to fd1"
    Right (Right ()) -> pure ()

err2 :: String -> Prog ()
err2 line = do
  Write (FD 2) line >>= \case
    Left NotWritable -> Trace "fd2 not writable"
    Right (Left EPIPE) -> Trace "EPIPE when writing to fd2"
    Right (Right ()) -> pure ()
