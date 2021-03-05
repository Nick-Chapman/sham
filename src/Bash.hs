
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
  [".",s] -> Source (Path.create s)

  ["ls"] -> Run Ls [] []
  ["ls",">>",p] -> Run Ls [] [Redirect OpenForAppending (FD 1) (FromPath (Path.create p))]

  ["echo",s] -> Run Echo [s] []
  ["echo",s1,s2] -> Run Echo [s1,s2] []
  ["echo",s,">>",p] -> Run Echo [s] [Redirect OpenForAppending (FD 1) (FromPath (Path.create p))]
  ["echo",s1,s2,">>",p] -> Run Echo [s1,s2] [Redirect OpenForAppending (FD 1) (FromPath (Path.create p))]

  ["cat",p1] -> Run Cat [p1] []
  ["cat",p1,">>",p2] -> Run Cat [p1] [Redirect OpenForAppending (FD 1) (FromPath (Path.create p2))]

  ["rev"] -> Run Rev [] []
  ["rev", "<", p] -> Run Rev [] [Redirect OpenForReading (FD 0) (FromPath (Path.create p))]
  ["rev", ">>", p] -> Run Rev [] [Redirect OpenForAppending (FD 1) (FromPath (Path.create p))]
  ["rev","<",p1,">>",p2] ->
    Run Rev [] [ Redirect OpenForReading (FD 0) (FromPath (Path.create p1))
               , Redirect OpenForAppending (FD 1) (FromPath (Path.create p2)) ]

  [s] -> Exec (Path.create s) []
  [s, "<", p] -> Exec (Path.create s) [Redirect OpenForReading (FD 0) (FromPath (Path.create p))]
  [s, ">>", p] -> Exec (Path.create s) [Redirect OpenForAppending (FD 1) (FromPath (Path.create p))]

  xs ->
    Echo2 ("bash, unable to parse: " ++ show xs)

data Script
  = Null
  | Echo2 String
  | Source Path
  | Run Builtin [String] [Redirect]
  | Exec Path [Redirect]

data Builtin = Echo | Cat | Rev | Ls


data Redirect
  = Redirect OpenMode FD RedirectSource

data RedirectSource
  = FromPath Path
--  | FromFD FD

interpret :: Script -> Prog ()
interpret = \case
  Null  -> pure ()
  Echo2 line -> write (FD 2) line -- TODO: use redirect
  Source path -> sourceProg path
  Run b args rs -> applyRedirects rs (builtinProg args b)
  Exec path rs -> applyRedirects rs (execWait path)


builtinProg :: [String] -> Builtin -> Prog ()
builtinProg args = \case
  Echo -> echoProg (unwords args)
  Cat -> catProg args
  Rev -> revProg -- ignores command line args
  Ls -> lsProg -- ignores command line args

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
    -- would be better if open readAll was in the scope of withOpen
    -- but withOpen doesn't have the right type: it's restricted to ()
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


catProg :: [String] -> Prog ()
catProg args =
  sequence_ [ catProg1 (Path.create arg) | arg <- args ]

catProg1 :: Path -> Prog ()
catProg1 path = do
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

-- TODO: use Exit for better type
withOpen :: Path -> OpenMode -> (FD -> Prog ()) -> Prog ()
withOpen path mode action =
  Open path mode >>= \case
    Left NoSuchPath -> err2 $ "no such path: " ++ Path.toString path
    Right fd -> do
      action fd
      Close fd

lsProg :: Prog ()
lsProg = do
  paths <- ListPaths
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
