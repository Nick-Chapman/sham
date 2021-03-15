module Script (
  Script(..), WaitMode(..),
  Step(..), Redirect(..), RedirectSource(..),
  Pred(..),Word(..),Var(..),
  run,
  runScript, Env(..)
  ) where

import MeNicks (Pid(..),Prog,Command(..),OpenMode(..))
import Prelude hiding (Word)
import SysCall (FD(..),SysCall(..), BadFileDescriptor(..), PipeEnds(..))
import qualified MeNicks (Prog(..))
import Native (err2,exit,stdin)
import qualified Native (echo,write,withOpen,readAll,read,write)
import qualified Path (create)

import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Interaction (Prompt(..))
import Misc (EOF(..))


run :: (String -> Maybe (Prog ())) -> ([String] -> Script) -> Script -> Prog ()
run lookNative shamParser script = do
  Command(com,args) <- MeNicks.Argv
  pid <- MeNicks.MyPid
  let bindings = Map.empty
  let env = Env { pid, com, args, bindings, lookNative, shamParser }
  runScript env script

data Env = Env
  { com :: String
  , args :: [String]
  , bindings :: Map Var String
  , pid :: Pid
  , lookNative :: String -> Maybe (Prog ())
  , shamParser :: [String] -> Script
  }

data Script
  = Null
  | ShamError String
  | Seq Script Script
  | If Pred Script Script
  | ReadIntoVar Var
  | Invoke1 Step WaitMode
  | Pipeline [Step] WaitMode
  deriving Show

data Pred
  = Eq Word Word
  | NotEq Word Word
  deriving Show

data WaitMode = Wait | NoWait
  deriving Show

data Step
  = Run Word [Word] [Redirect]
  | SubShell Script [Redirect]
  deriving Show

data Word
  = Word String
  | DollarHash
  | DollarN Int
  | DollarDollar
  | DollarName Var
  deriving Show

newtype Var = Var String
  deriving (Eq,Ord)

instance Show Var where show (Var s) = s

data Redirect = Redirect OpenMode FD RedirectSource
  deriving Show

data RedirectSource = FromPath Word | FromFD FD
  deriving Show

runScript :: Env -> Script -> Prog ()
runScript env0 script0 = loop env0 script0 (\_ -> pure ()) where

  loop :: Env -> Script -> (Env -> Prog ()) -> Prog ()
  loop env = \case
    ShamError s -> \_k -> do err2 s
    Seq s1 s2 -> \k -> loop env s1 (\env -> loop env s2 k)

    If pred s1 s2 -> \k -> do
      b <- evalPred env pred
      loop env (if b then s1 else s2) k

    ReadIntoVar x -> \k -> do
      line <- builtinRead
      let Env{bindings} = env
      k env { bindings = Map.insert x line bindings }

    Null -> \k -> do
      k env

    Invoke1 step mode -> \k -> do
      runStep env mode step
      k env

    Pipeline steps mode -> \k -> do
      runPipeline env mode steps
      k env

builtinRead :: Prog String
builtinRead =
  Native.read NoPrompt stdin >>= \case
    Left EOF -> exit
    Right line -> return line


runPipeline :: Env -> WaitMode -> [Step] -> Prog ()
runPipeline env wm steps = do
  case wm of
    NoWait -> err2 "runPipeline,NoWait" -- TODO
    _ -> do
      let progs = map (runStep env Wait) steps
      case progs of
        [] -> error "runPipeline[]"
        prog1:progs ->
          foldl pipe prog1 progs


pipe :: Prog () -> Prog () -> Prog ()
pipe prog1 prog2 = do
  PipeEnds{r,w} <- MeNicks.Call SysPipe ()
  let com1 = Command ("LEFT",[])
  let com2 = Command ("RIGHT",[])
  spawn1 com1 (do -- TODO: use the name of the actual pipe commands!
               dup2 (FD 1) w
               MeNicks.Call Close w
               MeNicks.Call Close r
               prog1
           ) $ \child1 -> do
    spawn1 com2 (do
                 dup2 (FD 0) r
                 MeNicks.Call Close w
                 MeNicks.Call Close r
                 prog2
             ) $ \child2 -> do
      MeNicks.Call Close w
      MeNicks.Call Close r
      MeNicks.Wait child1
      MeNicks.Wait child2

spawn1 :: Command -> Prog () -> (Pid -> Prog a) -> Prog a
spawn1 command child parent = do
  MeNicks.Fork >>= \case
    Nothing -> MeNicks.Exec command child
    Just pid -> parent pid


runStep :: Env -> WaitMode -> Step -> Prog ()
runStep env mode = \case
  Run w ws rs -> do
    com <- evalWord env w
    args <- mapM (evalWord env) ws
    runAct env (rs,mode) args (decode com)

  SubShell{} -> undefined


evalPred :: Env -> Pred -> Prog Bool
evalPred env = \case
  Eq w1 w2 -> do
    x1 <- evalWord env w1
    x2 <- evalWord env w2
    pure (x1 == x2)
  NotEq w1 w2 -> do
    x1 <- evalWord env w1
    x2 <- evalWord env w2
    pure (x1 /= x2)

evalWord :: Env -> Word -> Prog String
evalWord Env{pid,com,args,bindings} = \case
  Word s -> pure s
  DollarDollar -> let (Pid n) = pid in pure $ show n
  DollarHash -> pure $ show (length args)
  DollarN n ->
    if n > length args
    then do
      err2 ("$" ++ show n ++ " unbound")
      pure ""
    else pure $ (com:args)!!n
  DollarName x ->
    case Map.lookup x bindings of
      Nothing -> do
        err2 ("$" ++ show x ++ " unbound")
        pure ""
      Just v ->
        pure v

decode :: String -> Act
decode = \case
  "exit" -> Exit
  "echo" -> Echo
  "exec" -> Exec
  "." -> SourceSham
  name -> RunExternal name

data Act
  = Exit
  | Echo -- TODO: rename BuiltinEcho
  | SourceSham
  | RunExternal String
  | Exec

type Context = ([Redirect], WaitMode)

runAct :: Env -> Context -> [String] -> Act -> Prog ()
runAct env (rs,wm) args = \case

  Exit ->
    case (rs,wm,args) of
      ([],Wait,[]) -> MeNicks.Exit
      _ -> err2 "exit takes no args, redirects, or (&)"

  Echo ->
    -- TODO: check redirects/wait here & switch builtin/external echo
    echo env args (rs,wm)

  SourceSham -> do
    case (rs,wm,args) of
      ([],Wait,com:args) -> do
        script <- loadShamScript env com
        runScript env { args } script
      _ -> err2 "source takes at least one argument, but no redirects or (&)"

  RunExternal name -> do
    (command,prog) <- lookupCommand env name args
    runCommandInProcess env (rs,wm) command prog

  Exec -> do
    case wm of
      NoWait -> err2 "exec may not be run (&)"
      Wait -> do
        mapM_ (execRedirect env) rs
        case args of
          [] -> pure ()
          name:args -> do
            (command,prog) <- lookupCommand env name args
            MeNicks.Exec command prog


loadShamScript :: Env -> String -> Prog Script
loadShamScript env path = do
  lines <- do
    Native.withOpen (Path.create path) OpenForReading $ \fd -> do
      Native.readAll fd
  pure $ shamParser env lines


lookupCommand :: Env -> String -> [String] -> Prog (Command,Prog ())
lookupCommand env name args =
  case lookNative env name of
    Just prog -> pure (Command (name,args),prog)
    Nothing -> do
      case lookNative env "sham" of
        Nothing -> do err2 "cant find sham interpreter"; exit
        Just shamProg -> pure (Command ("sham",name:args),shamProg)


echo :: Env -> [String] -> Context -> Prog ()
echo env args = \case
  ([],Wait) ->
    Native.write (FD 1) (unwords args) --builtin echo
  context -> do
    runCommandInProcess env context (Command ("/bin/echo",args)) Native.echo

runCommandInProcess :: Env -> Context -> Command -> Prog () -> Prog ()
runCommandInProcess env (rs,mode) command prog = do
  MeNicks.Fork >>= \case
    Nothing -> do
      mapM_ (execRedirect env) rs
      MeNicks.Exec command prog
    Just pid -> case mode of
      Wait -> MeNicks.Wait pid
      NoWait -> pure ()


execRedirect :: Env -> Redirect -> Prog ()
execRedirect env = \case
  Redirect mode dest (FromPath path) -> do
    path <- evalWord env path
    Native.withOpen (Path.create path) mode $ \src -> do
      dup2 dest src
  Redirect _mode dest (FromFD src) -> do -- do we care what the mode is?
    dup2 dest src

dup2 :: FD -> FD -> Prog ()
dup2 d s = do
  MeNicks.Call Dup2 (d,s) >>= \case
    Left BadFileDescriptor -> do
      err2 $ "bad file descriptor: " ++ show s
      exit
    Right () -> pure ()
