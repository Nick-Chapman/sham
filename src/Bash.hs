
module Bash (console) where

import Data.List.Split (splitWhen)
import Data.Map (Map)
import Interaction (Prompt(..))
import Misc (EOF(..),PipeEnds(..))
import Native (Native,withOpen,err2)
import Os (Prog,SysCall(..),OpenMode(..),WriteOpenMode(..),FD(..))
import Path (Path)
import Prelude hiding (read)
import SysCall (BadFileDescriptor(..))
import qualified Data.Map.Strict as Map
import qualified Native (list,name,run,read)
import qualified Os (Prog(..))
import qualified Path (create)

console :: Prog ()
console = loop where
  loop :: Prog ()
  loop = do
    Native.read (Prompt "> ") (FD 0) >>= \case
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
    x1:xs ->
      foldl Pipe (parseCommand x1) (reverse [ parseCommand com | com <- xs ])

parseCommand :: String -> Script
parseCommand seg = loop [] [] (words seg)
  where
    loop ws rs xs =
      case parseAsRedirect xs of
        Just (r,xs) -> loop ws (r:rs) xs
        Nothing ->
          case xs of
            x:xs -> loop (x:ws) rs xs
            [] -> makeScript (reverse rs) (reverse ws)


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

makeScript :: [Redirect] -> [String] -> Script
makeScript rs = \case
  [] -> Null
  ["exit"] -> Exit
  "exit":_ -> BashError "exit: takes no arguments"
  [".",p] -> Source (Path.create p)
  ".":_ -> BashError "source (.): takes exactly one argument"
  com:args -> lookAmpersand args $ \args mode ->
    Run (makeCommand com) args rs mode

lookAmpersand :: [String] -> ([String] -> WaitMode -> a) -> a
lookAmpersand xs k =
  case reverse xs of
    "&":xs' -> k xs' NoWait
    _-> k xs Wait

makeCommand :: String -> Command
makeCommand str =
  case Map.lookup str nativeTable of
    Just native -> Left native
    Nothing -> Right (Path.create str)

nativeTable :: Map String Native
nativeTable = Map.fromList [ (Native.name x, x) | x <- Native.list ]


data Script
  = Null
  | BashError String
  -- TODO: sequencing operator ";"
  | Pipe Script Script
  | Exit
  | Source Path
--  | Exec Command -- TODO: need Exec from Os
  | Run Command [String] [Redirect] WaitMode

type Command = Either Native Path

data WaitMode = NoWait | Wait

data Redirect
  = Redirect OpenMode FD RedirectSource

data RedirectSource
  = FromPath Path
  | FromFD FD


interpret :: Script -> Prog ()
interpret = \case
  Null -> pure ()
  BashError message -> err2 message
  Exit -> Os.Exit
  Source path -> runBashScript path
  Run com args rs mode -> executeCommand com args rs mode
  Pipe script1 script2 -> pipe (interpret script1) (interpret script2)

pipe :: Prog () -> Prog () -> Prog ()
pipe prog1 prog2 = do
  PipeEnds{r,w} <- Os.Call SysPipe ()
  Os.Spawn (do
               dup2 (FD 1) w
               Os.Call Close w
               Os.Call Close r
               prog1
           ) $ \child1 -> do
    Os.Spawn (do
                 dup2 (FD 0) r
                 Os.Call Close w
                 Os.Call Close r
                 prog2
             ) $ \child2 -> do
      Os.Call Close w
      Os.Call Close r
      Os.Wait child1
      Os.Wait child2

executeCommand :: Command -> [String] -> [Redirect] -> WaitMode -> Prog ()
executeCommand com args rs mode = do
  spawn mode $ do
    mapM_ execRedirect rs
    case com of
      Left native -> Native.run native args
      Right path -> runBashScript path

runBashScript :: Path -> Prog ()
runBashScript path = do
  lines <- do
    withOpen path OpenForReading $ \fd -> do
      readAll fd
  sequence_ [ interpret (parseLine line) | line <- lines ]

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

readAll :: FD -> Prog [String]
readAll fd = loop []
  where
    loop acc =
      Native.read NoPrompt fd >>= \case
      Left EOF -> pure (reverse acc)
      Right line -> loop (line:acc)
