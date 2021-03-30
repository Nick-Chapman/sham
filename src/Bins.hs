-- | Predefined 'binary' programs, which will be available on the file-system.
module Bins (
  man, echo, cat, rev, grep, ls, mv, ps, lsof, sum, type_, xargs,
  ) where

import Control.Monad (when)
import Data.List (sort,sortOn,isInfixOf)
import Data.Map (Map)
import Interaction (Prompt(..))
import Lib (read,write,stdin,stdout,stderr,withOpen,checkNoArgs,checkAtLeastOneArg,getSingleArg,getTwoArgs,readAll,exit,execCommand)
import Misc (EOF(..))
import Path (Path)
import Prelude hiding (head,read,sum)
import Prog (Prog,Command(..),OpenMode(..),SysCall(..),FD,FileKind(..),BinaryMeta(..),NoSuchPath(..))
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import qualified Path (create,toString,hidden)
import qualified Prelude
import qualified Prog (Prog(..))

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
      , ("env"   , "(sham builtin) : display variable bindings")
      , ("exec"  , "(sham builtin) : replace the current process with a new command")
      , ("exit"  , "(sham builtin) : exit the current process")
      , ("grep"  , "copy stdin lines which match the given pattern to stdout ")
      , ("ls"    , "list non-hidden files; add '-a' flag to also see hidden files")
      , ("lsof"  , "list open files in running processes")
      , ("man"   , "show manual entries for commands, or show entry keys (no args)")
      , ("mv"    , "rename a file")
      , ("ps"    , "list running processes")
      , ("read"  , "(sham builtin) : read a line from stdin into a variable")
      , ("rev"   , "copy stdin to stdout, reversing each line")
      , ("sham"  , "run a script, or command (with '-c'), or start a new console (no args)")
      , ("source", "(sham builtin) : (also '.') interpret a script within the current shell")
      , ("sum"   , "write sum of numeric arguments to stdout")
      , ("type"  , "determine the type of named files: binary or data")
      , ("xargs" , "concatenate stdin lines, and pass to the given command")
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
  getSingleArg $ \pat -> do
  let
    loop :: Prog ()
    loop = do
      read NoPrompt stdin >>= \case
        Left EOF -> pure ()
        Right line -> do
          when (pat `isInfixOf` line) $
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
  Prog.Call Mv (Path.create src, Path.create dest) >>= \case
    Left NoSuchPath ->  write stderr $ "no such path: " ++ src
    Right () -> pure ()

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
  Command(_,args) <- Prog.Argv
  let
    typeline :: String -> Prog ()
    typeline name = do
      Prog.Call Kind (Path.create name) >>= \case
        Left _ ->  write stderr $ "no such path: " ++ name
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
