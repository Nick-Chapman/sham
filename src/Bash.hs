
module Bash (console) where

import Data.List (sort)
import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Os (Prog(..),SysCall(..),OpenMode(..),NoSuchPath(..),FD(..))
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

-- TODO: at some point we'll want a proper parser!
parseLine :: String -> Script
parseLine line = loop [] [] (words line)
  where
    loop ws rs xs =
      case parseAsRedirect xs of
        Just (r,xs) -> loop ws (r:rs) xs
        Nothing ->
          case xs of
            x:xs -> loop (x:ws) rs xs
            [] -> makeScript (reverse ws) (reverse rs)

    makeScript :: [String] -> [Redirect] -> Script
    makeScript ws rs =
      case (ws,rs) of
        (".":p:[],[]) -> Source (Path.create p)
        ("exit":[],[]) -> BashExit
        ("echo":args,_) -> Run Echo args rs
        ("cat":args,_) -> Run Cat args rs
        ("ls":[],_) -> Run Ls [] rs
        ("rev":[],_) -> Run Rev [] rs
        ([],[]) -> Null
        ([w],_) -> Exec (Path.create w) rs
        _ ->
          Echo2 ("bash, unable to parse: " ++ show line)

-- TODO: support '>' as 'rm/>>'
-- TODO: support fd 3,4.. (need 3 for swap stderr/stdout)
-- TODO: allow no space before path redirect: >xx
parseAsRedirect :: [String] -> Maybe (Redirect,[String])
parseAsRedirect = \case
  "<":p:xs -> Just (Redirect read (FD 0) (path p), xs)
  "0<":p:xs -> Just (Redirect read (FD 0) (path p), xs)
  ">>":p:xs -> Just (Redirect app (FD 1) (path p), xs)
  "1>>":p:xs -> Just (Redirect app (FD 1) (path p), xs)
  "2>>":p:xs -> Just (Redirect app (FD 2) (path p), xs)
  ">>&2":xs -> Just (Redirect app (FD 1) (dup 2), xs)
  "1>>&2":xs -> Just (Redirect app (FD 1) (dup 2), xs)
  "2>>&1":xs -> Just (Redirect app (FD 2) (dup 1), xs)
  _ ->
    Nothing
  where
    dup n = FromFD (FD n)
    path p = FromPath (Path.create p)
    read = OpenForReading
    app = OpenForAppending


data Script
  = Null
  | Echo2 String
  | Run Builtin [String] [Redirect]
  | Exec Path [Redirect]
  | Source Path
  | BashExit

data Builtin = Echo | Cat | Rev | Ls

data Redirect
  = Redirect OpenMode FD RedirectSource

data RedirectSource
  = FromPath Path
  | FromFD FD

interpret :: Script -> Prog ()
interpret = \case
  Null  -> pure ()
  Echo2 line -> write (FD 2) line -- TODO: use redirect
  Run b args rs -> executeBuiltin rs b args
  Exec path rs -> executePath rs path
  Source path -> runBashScript path
  BashExit -> Exit

executePath :: [Redirect] -> Path -> Prog ()
executePath rs path = spawnWait (execRedirects rs (runBashScript path))

executeBuiltin :: [Redirect] -> Builtin -> [String] -> Prog ()
executeBuiltin rs b args = spawnWait (execRedirects rs (builtinProg args b))

spawnWait :: Prog () -> Prog ()
spawnWait prog = do Spawn prog (\childPid -> Wait childPid)

execRedirects :: [Redirect] -> Prog () -> Prog ()
execRedirects = \case
  [] -> \prog -> prog
  r:rs -> execRedirect r . execRedirects rs

execRedirect :: Redirect -> Prog () -> Prog ()
execRedirect r prog = -- TODO: doesn't need to take prog..
  case r of
    Redirect mode dFd (FromPath path) -> do
      withOpen path mode $ \sFd -> do
        Call Dup2 (dFd,sFd)
        prog
    Redirect _mode dFd (FromFD sFd) -> do -- do we care what the mode is?
      Call Dup2 (dFd,sFd)
      prog -- ..very easy to forget & cause a bug hunt!

runBashScript :: Path -> Prog ()
runBashScript path = do
  lines <- do
    withOpen path OpenForReading $ \fd -> do
      readAll fd
  sequence_ [ interpret (parseLine line) | line <- lines ]

readAll :: FD -> Prog [String]
readAll fd = loop []
  where
    loop acc =
      read fd >>= \case
      Left EOF -> pure (reverse acc)
      Right line -> loop (line:acc)

builtinProg :: [String] -> Builtin -> Prog ()
builtinProg args = \case
  Echo -> echoProg (unwords args)
  Cat -> catProg args
  Rev -> revProg -- ignores command line args
  Ls -> lsProg -- ignores command line args

echoProg :: String -> Prog ()
echoProg line = write (FD 1) line

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

lsProg :: Prog ()
lsProg = do
  paths <- Call Paths ()
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

withOpen :: Path -> OpenMode -> (FD -> Prog a) -> Prog a
withOpen path mode action =
  Call Open (path,mode) >>= \case
    Left NoSuchPath -> do
      err2 $ "no such path: " ++ Path.toString path
      Exit
    Right fd -> do
      res <- action fd
      Call Close fd
      pure res

read :: FD -> Prog (Either EOF String)
read fd =
  Call Read fd >>= \case
    Left NotReadable -> do
      err2 "fd0 not readable"
      pure (Left EOF) -- TODO: better to exit?
    Right eofOrLine -> do
      pure eofOrLine

write :: FD -> String -> Prog ()
write fd line = do
  Call Write (fd,line) >>= \case
    Left NotWritable -> err2 "fd1 not writable"
    Right (Left EPIPE) -> err2 "EPIPE when writing to fd1"
    Right (Right ()) -> pure ()

err2 :: String -> Prog ()
err2 line = do
  Call Write (FD 2, line) >>= \case
    Left NotWritable -> Trace "fd2 not writable"
    Right (Left EPIPE) -> Trace "EPIPE when writing to fd2"
    Right (Right ()) -> pure ()
