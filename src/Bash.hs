
module Bash (console) where

import Data.List (sort)
import Data.List.Split (splitWhen)
import Interaction (Prompt(..))
import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Os (Prog,SysCall(..),OpenMode(..),WriteOpenMode(..),NoSuchPath(..),FD(..))
import Path (Path)
import Prelude hiding (read)
import SysCall (BadFileDescriptor(..),PipeEnds(..))
import qualified Os (Prog(..))
import qualified Path (create,toString)

console :: Prog ()
console = loop where
  loop :: Prog ()
  loop = do
    read (Prompt "> ") (FD 0) >>= \case
      Left EOF -> pure ()
      Right line -> do
        interpret (parseLine line)
        loop

-- TODO: at some point we'll want a proper parser!
-- TODO: allow no space before path redirect: >xx
parseLine :: String -> Script
parseLine line = do
  case splitWhen (=='|') line of
    [] -> error "parseLine/split/[]/impossible"
    [seg] -> parseCommand seg
    [seg1,seg2] -> Pipe (parseCommand seg1) (parseCommand seg2)
    [seg1,seg2,seg3] -> Pipe (Pipe (parseCommand seg1) (parseCommand seg2)) (parseCommand seg3)
    _ ->
      error "multi-pipeline-not-supported-yet" -- TODO: multi pipes

parseCommand :: String -> Script
parseCommand seg = loop [] [] (words seg)
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
        ("exit":[],[]) -> Exit
        ("echo":args,_) -> makeBuiltin Echo args rs
        ("cat":args,_) -> makeBuiltin Cat args rs
        ("ls":args,_) -> makeBuiltin Ls args rs
        ("ps":args,_) -> makeBuiltin Ps args rs
        ("rev":args,_) -> makeBuiltin Rev args rs
        ("exec":args,_) -> makeBuiltin Exec args rs
        ([],[]) -> Null
        (args,_) -> makeBuiltin Exec args rs

makeBuiltin :: Builtin -> [String] -> [Redirect] -> Script
makeBuiltin b args rs =
  lookAmpersand args $ \args mode -> Command b args rs mode

lookAmpersand :: [String] -> ([String] -> WaitMode -> a) -> a
lookAmpersand xs k =
  case reverse xs of
    "&":xs' -> k xs' NoWait
    _-> k xs Wait

-- TODO: generalize/share redirect parsing; even before a proper parser
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

  -- FD 4...
  ">&4":xs -> Just (Redirect wr (FD 1) (dup 4), xs)
  "1>&4":xs -> Just (Redirect wr (FD 1) (dup 4), xs)
  "2>&4":xs -> Just (Redirect wr (FD 2) (dup 4), xs)
  "4>&1":xs -> Just (Redirect wr (FD 4) (dup 1), xs)
  "4>&2":xs -> Just (Redirect wr (FD 4) (dup 2), xs)
  "4<":p:xs -> Just (Redirect rd (FD 4) (path p), xs)

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
  -- TODO: sequencing operator ";"
  | Command Builtin [String] [Redirect] WaitMode
  | Source Path -- TODO: builtin
  | Exit -- TODO: builtin
  | Pipe Script Script

data WaitMode = NoWait | Wait

data Builtin = Exec | Echo | Cat | Rev | Ls | Ps

data Redirect
  = Redirect OpenMode FD RedirectSource

data RedirectSource
  = FromPath Path
  | FromFD FD

interpret :: Script -> Prog ()
interpret = \case
  Null  -> pure ()
  Exit -> Os.Exit
  Source path -> runBashScript path
  Command b args rs waitMode -> executeBuiltin rs b args waitMode

  Pipe script1 script2 -> do
    PipeEnds{r,w} <- Os.Call SysPipe ()
    Os.Spawn (do
                 dup2 (FD 1) w
                 Os.Call Close w
                 Os.Call Close r
                 interpret script1
             ) $ \child1 -> do
      Os.Spawn (do
                   dup2 (FD 0) r
                   Os.Call Close w
                   Os.Call Close r
                   interpret script2
               ) $ \child2 -> do
        Os.Call Close w
        Os.Call Close r
        Os.Wait child1
        Os.Wait child2


executeBuiltin :: [Redirect] -> Builtin -> [String] -> WaitMode -> Prog ()
executeBuiltin rs b args mode = do
  spawn mode $ do
    mapM_ execRedirect rs
    builtinProg args b

spawn :: WaitMode -> Prog () -> Prog ()
spawn mode prog = case mode of
    Wait -> do Os.Spawn prog (\childPid -> Os.Wait childPid)
    NoWait -> do Os.Spawn prog (\_ -> pure ())

execRedirect :: Redirect -> Prog ()
execRedirect r =
  case r of
    Redirect mode dest (FromPath path) -> do
      withOpen path mode $ \src -> do
        dup2 dest src
    Redirect _mode dest (FromFD src) -> do -- do we care what the mode is?
      dup2 dest src

dup2 :: FD -> FD -> Prog ()
dup2 d s = do
  Os.Call Dup2 (d,s) >>= \case
    Left BadFileDescriptor -> do
      err2 $ "bad file descriptor: " ++ show s
      Os.Exit
    Right () -> pure ()

builtinProg :: [String] -> Builtin -> Prog ()
builtinProg args = \case
  Exec -> execProg args
  Echo -> echoProg (unwords args)
  Cat -> catProg args
  Rev -> revProg -- ignores command line args
  Ls -> lsProg -- ignores command line args
  Ps -> psProg

execProg :: [String] -> Prog ()
execProg = \case
  [] -> err2 "execProg/0"
  [p] -> do
    runBashScript (Path.create p)
  _ ->
    err2 "execProg/multi"

runBashScript :: Path -> Prog ()
runBashScript path = do
  lines <- do
    withOpen path OpenForReading $ \fd -> do
      readAll fd
  sequence_ [ interpret (parseLine line) | line <- lines ]

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
    read NoPrompt fd >>= \case
      Left EOF -> pure ()
      Right line -> do
        write (FD 1) line
        loop

revProg :: Prog ()
revProg = loop where
  loop :: Prog ()
  loop = do
    read NoPrompt (FD 0) >>= \case
      Left EOF -> pure ()
      Right line -> do
        write (FD 1) (reverse line)
        loop

lsProg :: Prog ()
lsProg = do
  paths <- Os.Call Paths ()
  mapM_ (write (FD 1) . Path.toString) (sort paths)

psProg :: Prog ()
psProg = do
  pids <- Os.Pids
  mapM_ (write (FD 1) . show) (sort pids)

withOpen :: Path -> OpenMode -> (FD -> Prog a) -> Prog a
withOpen path mode action =
  Os.Call Open (path,mode) >>= \case
    Left NoSuchPath -> do
      err2 $ "no such path: " ++ Path.toString path
      Os.Exit
    Right fd -> do
      res <- action fd
      Os.Call Close fd
      pure res

readAll :: FD -> Prog [String]
readAll fd = loop []
  where
    loop acc =
      read NoPrompt fd >>= \case
      Left EOF -> pure (reverse acc)
      Right line -> loop (line:acc)

read :: Prompt -> FD -> Prog (Either EOF String)
read prompt fd =
  Os.Call (Read prompt) fd >>= \case
    Left NotReadable -> do
      err2 (show fd ++ " not readable")
      pure (Left EOF) -- TODO: better to exit?
    Right eofOrLine -> do
      pure eofOrLine

write :: FD -> String -> Prog ()
write fd line = do
  Os.Call Write (fd,line) >>= \case
    Left NotWritable -> err2 (show fd ++ " not writable")
    Right (Left EPIPE) -> err2 "EPIPE when writing to fd1"
    Right (Right ()) -> pure ()

err2 :: String -> Prog ()
err2 line = do
  Os.Call Write (FD 2, line) >>= \case
    Left NotWritable -> Os.Trace (show (FD 2) ++ " not writable")
    Right (Left EPIPE) -> Os.Trace "EPIPE when writing to fd2"
    Right (Right ()) -> pure ()
