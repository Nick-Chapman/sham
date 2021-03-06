
module Bash (console) where

import Data.List (sort)
import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Os (Prog(..),SysCall(..),OpenMode(..),WriteOpenMode(..),NoSuchPath(..),FD(..))
import SysCall (BadFileDescriptor(..))
import Path (Path)
import Prelude hiding (read)
import qualified Path (create,toString)

console :: Prog ()
console = loop where
  loop :: Prog ()
  loop = do
    --write (FD 1) "prompt $"
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
        ("echo":args,_) -> makeBuiltin Echo args rs
        ("cat":args,_) -> makeBuiltin Cat args rs
        ("ls":args,_) -> makeBuiltin Ls args rs
        ("rev":args,_) -> makeBuiltin Rev args rs
        ([],[]) -> Null
        ([w],_) -> Exec (Path.create w) rs
        _ ->
          Echo2 ("bash, unable to parse: " ++ show line)


makeBuiltin :: Builtin -> [String] -> [Redirect] -> Script
makeBuiltin b args rs =
  case reverse args of
    "&":args' -> Run b (reverse args') rs (Just NoWait)
    _-> Run b args rs Nothing


-- TODO: support fd 3,4.. (need 3 for swap stderr/stdout)
-- TODO: allow no space before path redirect: >xx
parseAsRedirect :: [String] -> Maybe (Redirect,[String])
parseAsRedirect = \case
  "<":p:xs -> Just (Redirect rd (FD 0) (path p), xs)
  "0<":p:xs -> Just (Redirect rd (FD 0) (path p), xs)

  ">":p:xs -> Just (Redirect wr (FD 1) (path p), xs)
  "1>":p:xs -> Just (Redirect wr (FD 1) (path p), xs)
  "2>":p:xs -> Just (Redirect wr (FD 2) (path p), xs)
  ">&2":xs -> Just (Redirect wr (FD 1) (dup 2), xs)
  "1>&2":xs -> Just (Redirect wr (FD 1) (dup 2), xs)
  "2>&1":xs -> Just (Redirect wr (FD 2) (dup 1), xs)

  ">>":p:xs -> Just (Redirect ap (FD 1) (path p), xs)
  "1>>":p:xs -> Just (Redirect ap (FD 1) (path p), xs)
  "2>>":p:xs -> Just (Redirect ap (FD 2) (path p), xs)
  ">>&2":xs -> Just (Redirect ap (FD 1) (dup 2), xs)
  "1>>&2":xs -> Just (Redirect ap (FD 1) (dup 2), xs)
  "2>>&1":xs -> Just (Redirect ap (FD 2) (dup 1), xs)

  -- provoke unusual conditions
  "0>":p:xs -> Just (Redirect wr (FD 0) (path p), xs)
  ">&0":xs -> Just (Redirect wr (FD 1) (dup 0), xs)
  "1>&0":xs -> Just (Redirect wr (FD 1) (dup 0), xs)
  "2>&0":xs -> Just (Redirect wr (FD 2) (dup 0), xs)

  -- FD 3...
  ">&3":xs -> Just (Redirect wr (FD 1) (dup 3), xs)
  "1>&3":xs -> Just (Redirect wr (FD 1) (dup 3), xs)
  "2>&3":xs -> Just (Redirect wr (FD 2) (dup 3), xs)
  "3>&1":xs -> Just (Redirect wr (FD 3) (dup 1), xs)
  "3>&2":xs -> Just (Redirect wr (FD 3) (dup 2), xs)
  "3<":p:xs -> Just (Redirect rd (FD 3) (path p), xs)

  _ ->
    Nothing
  where
    dup n = FromFD (FD n)
    path p = FromPath (Path.create p)
    rd = OpenForReading
    wr = OpenForWriting Truncate
    ap = OpenForWriting Append


data Script
  = Null
  | Echo2 String
  | Run Builtin [String] [Redirect] (Maybe NoWait)
  | Exec Path [Redirect] -- TODO: NoWait on Exec
  | Source Path
  | BashExit
  -- TODO: sequencing operator ";"
  -- TODO: pipe operator "|"

data NoWait = NoWait

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
  Run b args rs waitMode -> executeBuiltin rs b args waitMode
  Exec path rs -> executePath rs path
  Source path -> runBashScript path
  BashExit -> Exit

executePath :: [Redirect] -> Path -> Prog ()
executePath rs path = spawnWait (execRedirects rs (runBashScript path))

executeBuiltin :: [Redirect] -> Builtin -> [String] -> Maybe NoWait -> Prog ()
executeBuiltin rs b args = \case
  Nothing -> spawnWait (execRedirects rs (builtinProg args b))
  Just NoWait -> spawn (execRedirects rs (builtinProg args b))

spawn :: Prog () -> Prog ()
spawn prog = do Spawn prog (\_ -> pure ())

spawnWait :: Prog () -> Prog ()
spawnWait prog = do Spawn prog (\childPid -> Wait childPid)

execRedirects :: [Redirect] -> Prog () -> Prog ()
execRedirects = \case
  [] -> \prog -> prog
  r:rs -> execRedirect r . execRedirects rs

execRedirect :: Redirect -> Prog () -> Prog ()
execRedirect r prog = -- TODO: doesn't need to take prog..
  case r of
    Redirect mode dest (FromPath path) -> do
      withOpen path mode $ \src -> do
        dup2 dest src
        prog
    Redirect _mode dest (FromFD src) -> do -- do we care what the mode is?
      dup2 dest src
      prog -- ..very easy to forget & cause a bug hunt!


dup2 :: FD -> FD -> Prog ()
dup2 d s = do
  Call Dup2 (d,s) >>= \case
    Left BadFileDescriptor -> do
      err2 $ "bad file descriptor: " ++ show s
      Exit
    Right () -> pure ()

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
catProg = \case
  [] ->
    catFd (FD 0)
  args ->
    sequence_ [ catProg1 (Path.create arg) | arg <- args ]

catProg1 :: Path -> Prog ()
catProg1 path = withOpen path OpenForReading $ catFd

catFd :: FD -> Prog ()
catFd fd = loop where
  loop :: Prog ()
  loop = do
    read fd >>= \case
      Left EOF -> pure ()
      Right line -> do
        write (FD 1) line
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
      err2 (show fd ++ " not readable")
      pure (Left EOF) -- TODO: better to exit?
    Right eofOrLine -> do
      pure eofOrLine

write :: FD -> String -> Prog ()
write fd line = do
  Call Write (fd,line) >>= \case
    Left NotWritable -> err2 (show fd ++ " not writable")
    Right (Left EPIPE) -> err2 "EPIPE when writing to fd1"
    Right (Right ()) -> pure ()

err2 :: String -> Prog ()
err2 line = do
  Call Write (FD 2, line) >>= \case
    Left NotWritable -> Trace (show (FD 2) ++ " not writable")
    Right (Left EPIPE) -> Trace "EPIPE when writing to fd2"
    Right (Right ()) -> pure ()
