
module Native (
  Native,list,name,run,
  withOpen,read,write,err2
  ) where

import Data.List (sort,sortOn)
import Interaction (Prompt(..))
import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Os (Prog,OpenMode(..),NoSuchPath(..))
import Path (Path)
import Prelude hiding (all,read)
import SysCall (SysCall(..),FD)
import qualified Os
import qualified Path (create,toString)

list :: [Native]
name :: Native -> String
run :: Native -> [String] -> Prog ()

data Native = Echo | Cat | Rev | Ls | Ps | Builtins
  deriving (Bounded,Enum)

binding :: Native -> (String, [String] -> Prog ())
binding = \case
  Echo -> ("echo",echoProg)
  Cat -> ("cat",catProg)
  Rev -> ("rev",revProg)
  Ls -> ("ls",lsProg)
  Ps -> ("ps",psProg)
  Builtins -> ("builtins",builtinsProg)

list = [minBound..maxBound]
name = fst . binding
run n args = snd (binding n) args

echoProg :: [String] -> Prog ()
echoProg args = write stdout (unwords args)

catProg :: [String] -> Prog ()
catProg = \case
  [] ->
    catFd stdin
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
        write stdout line
        loop

revProg :: [String] -> Prog ()
revProg args = checkNoArgs "rev" args loop where
  loop :: Prog ()
  loop = do
    read NoPrompt stdin >>= \case
      Left EOF -> pure ()
      Right line -> do
        write stdout (reverse line)
        loop

lsProg :: [String] -> Prog ()
lsProg args = checkNoArgs "ls" args $ do
  paths <- Os.Call Paths ()
  mapM_ (write stdout . Path.toString) (sort paths)

psProg :: [String] -> Prog ()
psProg args = checkNoArgs "ps" args $ do
  xs <- Os.Procs
  sequence_
    [ write stdout (show pid ++ " " ++ show p)  | (pid,p) <- sortOn fst xs ]

builtinsProg :: [String] -> Prog ()
builtinsProg args = checkNoArgs "builtins" args $ do
  mapM_ (write stdout) $ sort [ (name x) | x <- list ]


checkNoArgs :: String -> [String] -> Prog () -> Prog ()
checkNoArgs who args prog = case args of
  [] -> prog
  _ -> err2 (who ++ ": takes no arguments")

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
  Os.Call Write (stderr, line) >>= \case
    Left NotWritable -> Os.Trace (show stderr ++ " not writable")
    Right (Left EPIPE) -> Os.Trace "EPIPE when writing to fd2"
    Right (Right ()) -> pure ()

stdin,stdout,stderr :: FD
stdin = 0
stdout = 1
stderr = 2
