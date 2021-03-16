module Native (
  echo, cat, rev, grep, ls, ps, bins, xargs, man, sum, type_,
  loadFile, withOpen, readAll, read, write, err2, checkNoArgs, exit,
  stdin, stdout,
  ) where

import Control.Monad (when)
import Data.List (sort,sortOn,isInfixOf)
import Data.Map (Map)
import Interaction (Prompt(..))
import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Path (Path)
import Prelude hiding (head,read,sum)
import Prog (Prog,Command(..),OpenMode(..),SysCall(..),FD,OpenError(..),FileKind(..),BinaryMeta(..))
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import qualified Path (create,toString)
import qualified Prelude
import qualified Prog (Prog(..))

type_ :: Prog ()
type_ = do
  Command(_,args) <- Prog.Argv
  let
    typeline :: String -> Prog ()
    typeline name = do
      Prog.Call Kind (Path.create name) >>= \case
        Left _ ->  err2 $ "no such path: " ++ name
        Right kind -> do
          let
            str = case kind of
              K_Data -> "Data/Script"
              K_Binary (BinaryMeta s) -> "Binary *" ++ s ++ "*"
          write stdout (name ++ " : " ++ str)
  mapM_ typeline args

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
ls = checkNoArgs $ do
  paths <- Prog.Call Paths ()
  mapM_ (write stdout . Path.toString) (sort paths)

ps :: Prog ()
ps = checkNoArgs $ do
  xs <- Prog.Procs
  sequence_
    [ write stdout (show pid ++ " " ++ show com)  | (pid,com) <- sortOn fst xs ]

bins :: [String] -> Prog ()
bins names = checkNoArgs $ do
  mapM_ (write stdout) $ names

xargs :: (Command -> Prog ()) -> Prog ()
xargs runCommand = do
  Command(me,args) <- Prog.Argv
  case args of
    [] -> err2 (me ++ ": takes at least 1 argument")
    com:args -> do
      lines <- readAll stdin
      runCommand (Command (com, args ++ lines))

man :: Prog ()
man = do
  Command(me,args) <- Prog.Argv
  let
    manline :: String -> Prog ()
    manline name =
      case Map.lookup name docsMap of
        Just text -> write stdout (name ++ " : " ++ text)
        Nothing -> write stderr (me ++ " : no manual entry for '" ++ name ++ "'")
  mapM_ manline args
  where
    docsMap :: Map String String
    docsMap = Map.fromList
     [ ("echo" , "write given arguments to stdout")
     , ("cat"  , "write named files (or stdin in no files given) to stdout")
     , ("rev"  , "copy stdin to stdout, reversing each line")
     , ("grep" , "copy lines which match the given pattern to stdout ")
     , ("ls"   , "list all files on the filesystem")
     , ("ps"   , "list all running process")
     , ("sham" , "start a nested sham console")
     , ("xargs", "concatenate lines from stdin, and pass as arguments to the given command")
     , ("man"  , "list the manual entries for the given commands")
     , ("sum"  , "write sum of the given numeric arguments to stdout")
     , ("type" , "determine kind of named files: binary or data")
     ]


sum :: Prog ()
sum = do
  Command(me,args) <- Prog.Argv
  let
    toInt :: String -> Prog Int
    toInt s =
      case readMaybe s of
        Just n -> pure n
        Nothing -> do
          err2 (me ++ ": unable to convert '" ++ s ++ "' to a number")
          pure 0
  ns <- mapM toInt args
  let res = Prelude.sum ns
  write stdout (show res)

checkNoArgs :: Prog () -> Prog ()
checkNoArgs prog = do
  Command(com,args) <- Prog.Argv
  case args of
    [] -> prog
    _ -> err2 (com ++ ": takes no arguments")

getSingleArg :: (String -> Prog ()) -> Prog ()
getSingleArg f = do
  Command(com,args) <- Prog.Argv
  case args of
    [arg] -> f arg
    _ -> err2 (com ++ ": takes a single argument")

loadFile :: String -> Prog [String]
loadFile path = do
  withOpen (Path.create path) OpenForReading $ \fd -> do
    readAll fd

withOpen :: Path -> OpenMode -> (FD -> Prog a) -> Prog a
withOpen path mode action =
  Prog.Call Open (path,mode) >>= \case
    Left OE_NoSuchPath -> do
      err2 $ "no such path: " ++ Path.toString path
      exit
    Left OE_CantOpenForReading -> do
      err2 $ "cant open for reading: " ++ Path.toString path
      exit
    Right fd -> do
      res <- action fd
      Prog.Call Close fd
      pure res

readAll :: FD -> Prog [String]
readAll fd = loop []
  where
    loop acc =
      Native.read NoPrompt fd >>= \case
      Left EOF -> pure (reverse acc)
      Right line -> loop (line:acc)

read :: Prompt -> FD -> Prog (Either EOF String)
read prompt fd =
  Prog.Call (Read prompt) fd >>= \case
    Left NotReadable -> do err2 (show fd ++ " not readable"); exit
    Right eofOrLine -> pure eofOrLine

write :: FD -> String -> Prog ()
write fd line = do
  Prog.Call Write (fd,line) >>= \case
    Left NotWritable -> do err2 (show fd ++ " not writable"); exit
    Right (Left EPIPE) -> exit
    Right (Right ()) -> pure ()

err2 :: String -> Prog ()
err2 line = do
  Prog.Call Write (stderr, line) >>= \case
    Left NotWritable -> Prog.Trace (show stderr ++ " not writable")
    Right (Left EPIPE) -> Prog.Trace "EPIPE when writing to fd2"
    Right (Right ()) -> pure ()

exit :: Prog a
exit = Prog.Exit

stdin,stdout,stderr :: FD
stdin = 0
stdout = 1
stderr = 2
