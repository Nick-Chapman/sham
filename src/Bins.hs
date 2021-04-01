-- | Predefined 'binary' programs, which will be available on the file-system.
module Bins (
  man, echo, env, cat, rev, grep, ls, kill, mv, rm, ps, lsof, sum, type_, xargs,
  ) where

import Control.Monad (when)
import Data.List (sort,sortOn,isInfixOf)
import Data.Map (Map)
import Environment (Var(..),bindings)
import Interaction (Prompt(..),EOF(..))
import Lib (read,write,stdin,stdout,stderr,withOpen,checkNoArgs,checkAtLeastOneArg,getTwoArgs,readAll,exit,execCommand)
import Path (Path)
import Prelude hiding (head,read,sum)
import Prog
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import qualified Path (create,toString,hidden)
import qualified Prelude

man :: Prog ()
man = do
  Command(me,args) <- Prog.Argv
  let
    manline :: String -> Prog ()
    manline name =
      case Map.lookup name docsMap of
        Just text -> write stdout (name ++ " : " ++ text)
        Nothing -> write stderr (me ++ " : no manual entry for '" ++ name ++ "'")

  case args of
    [] -> write stdout (unwords [ k | k <- Map.keys docsMap])
    args -> mapM_ manline args

  where
    docsMap :: Map String String
    docsMap = Map.fromList
      [ ("cat"   , "write named files (or stdin in no files named) to stdout")
      , ("echo"  , "(binary/sham builtin) write arguments to stdout")
      , ("env"   , "list variable bindings")
      , ("exec"  , "(sham builtin) : replace the current process with a new command")
      , ("exit"  , "(sham builtin) : exit the current process")
      , ("grep"  , "copy stdin lines to stdout which match (-v dont match) the pattern")
      , ("kill"  , "kill processes by pid")
      , ("ls"    , "list non-hidden files; add '-a' flag to also see hidden files")
      , ("lsof"  , "list open files in running processes")
      , ("man"   , "show manual entries for commands, or show entry keys (no args)")
      , ("mv"    , "rename a file")
      , ("ps"    , "list running processes")
      , ("read"  , "(sham builtin) : read a line from stdin into a variable")
      , ("rev"   , "copy stdin to stdout, reversing each line")
      , ("rm"    , "rm multiple files")
      , ("sham"  , "run a script, or command (with '-c'), or start a new console (no args)")
      , ("source", "(sham builtin) : (also '.') interpret a script within the current shell")
      , ("sum"   , "write sum of numeric arguments to stdout")
      , ("type"  , "determine the type of named files: binary or data")
      , ("xargs" , "concatenate stdin lines, and pass to the given command")
      ]

env :: Prog ()
env = do
  environment <- Prog.MyEnvironment
  sequence_ [ write stdout (k ++ "=" ++ v)
            | (Var k,v) <- Environment.bindings environment
            ]

echo :: Prog ()
echo = do
  Command(_,args) <- Prog.Argv
  write stdout (unwords args)

cat :: Prog ()
cat = do
  Command(_,args) <- Prog.Argv
  case args of
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

rev :: Prog ()
rev = checkNoArgs loop where
  loop :: Prog ()
  loop = do
    read NoPrompt stdin >>= \case
      Left EOF -> pure ()
      Right line -> do
        write stdout (reverse line)
        loop

grep :: Prog ()
grep = do
  Command(me,args) <- Prog.Argv
  case (case args of [pat] -> Just (True,pat)
                     ["-v",pat] -> Just (False,pat)
                     _ -> Nothing
       ) of
    Nothing -> write stderr (me ++ ": takes optional '-v' and one more argument")
    Just (sense,pat) -> do
      let
        loop :: Prog ()
        loop = do
          read NoPrompt stdin >>= \case
            Left EOF -> pure ()
            Right line -> do
              when ((pat `isInfixOf` line) == sense) $
                write stdout line
              loop
      loop

ls :: Prog ()
ls = do
  Command(me,args) <- Prog.Argv
  seeHidden <- case args of
    [] -> pure False
    ["-a"] -> pure True
    _ -> do write stderr (me ++ ": takes no arguments, or a single '-a'"); exit
  paths <- Prog.Call Paths ()
  mapM_ (write stdout . Path.toString) $ sort [ p | p <- paths , seeHidden || not (Path.hidden p) ]

mv :: Prog ()
mv = getTwoArgs $ \src dest -> do
  Command(me,_) <- Prog.Argv
  Prog.Call Mv (Path.create src, Path.create dest) >>= \case
    Left NoSuchPath ->  write stderr (me ++ ": no such path: " ++ src)
    Right () -> pure ()

rm :: Prog ()
rm = do
  Command(me,args) <- Prog.Argv
  let
    rm1 :: String -> Prog ()
    rm1 name = do
      Prog.Call Rm (Path.create name) >>= \case
        Left NoSuchPath ->  write stderr (me ++ ": no such path: " ++ name)
        Right () -> pure ()
  mapM_ rm1 args

kill :: Prog ()
kill = checkAtLeastOneArg $ do
  Command(me,args) <- Prog.Argv
  let
    k1 :: String -> Prog ()
    k1 arg = do
      case readMaybe arg of
        Nothing -> do
          write stderr (me ++ ": not a pid: " ++ arg)
        Just n -> do
          let pid = Pid n
          Prog.Kill pid >>= \case
            Left NoSuchProcess ->  write stderr (me ++ ": no such process: " ++ show pid)
            Right () -> pure ()
  mapM_ k1 args

ps :: Prog ()
ps = checkNoArgs $ do
  xs <- Prog.Procs
  sequence_
    [ write stdout (show pid ++ " " ++ show com)  | (pid,com) <- sortOn fst xs ]

lsof :: Prog ()
lsof = checkNoArgs $ do
  xs <- Prog.Lsof
  sequence_
    [ write stdout (show pid ++ " (" ++ show command ++ ") " ++ show fd ++ " " ++ show entry)
    | (pid,command,fd,entry) <- xs ]

sum :: Prog ()
sum = do
  Command(me,args) <- Prog.Argv
  let
    toInt :: String -> Prog Int
    toInt s =
      case readMaybe s of
        Just n -> pure n
        Nothing -> do
          write stderr (me ++ ": unable to convert '" ++ s ++ "' to a number")
          pure 0
  ns <- mapM toInt args
  let res = Prelude.sum ns
  write stdout (show res)

type_ :: Prog ()
type_ = checkAtLeastOneArg $ do
  Command(me,args) <- Prog.Argv
  let
    typeline :: String -> Prog ()
    typeline name = do
      Prog.Call Kind (Path.create name) >>= \case
        Left _ ->  write stderr (me ++ ": no such path: " ++ name)
        Right kind -> do
          let
            str = case kind of
              K_Data -> "Data/Script"
              K_Binary (BinaryMeta s) -> "Binary *" ++ s ++ "*"
          write stdout (name ++ " : " ++ str)
  mapM_ typeline args

xargs :: Prog ()
xargs = checkAtLeastOneArg $ do
  Command(_,args) <- Prog.Argv
  environment <- Prog.MyEnvironment
  case args of
    [] -> error "impossible"
    com:args -> do
      lines <- readAll stdin
      let words = lines >>= Prelude.words
      execCommand environment (Command (com, args ++ words))
