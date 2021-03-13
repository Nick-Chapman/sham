module Script (
  Script(..), WaitMode(..),
  Step(..), Redirect(..), RedirectSource(..),
  Word(..),
  run,
  runScript, Env(..)
  ) where

import MeNicks (Pid(..),Prog,Command(..),OpenMode(..))
import Prelude hiding (Word)
import SysCall (FD(..),SysCall(..), BadFileDescriptor(..), PipeEnds(..))
import qualified MeNicks (Prog(..))
import Native (err2)
import qualified Native (echo,write,withOpen,readAll)
import qualified Path (create)

run :: (String -> Maybe (Prog ())) -> ([String] -> Script) -> Script -> Prog ()
run lookNative shamParser script = do
  Command(com,args) <- MeNicks.Argv
  pid <- MeNicks.MyPid
  let env = Env { pid, com, args, lookNative, shamParser }
  runScript env script

data Env = Env
  { com :: String
  , args :: [String]
  , pid :: Pid
  , lookNative :: String -> Maybe (Prog ())
  , shamParser :: [String] -> Script
  }

data Script
  = Null
  | ShamError String
  | Seq Script Script
  | IfEq Word Word Script Script
  | Invoke1 Step WaitMode
  | Pipeline [Step] WaitMode
  deriving Show

data WaitMode = Wait | NoWait
  deriving Show

data Step
  = Run Word [Word] [Redirect]
  | SubShell Script [Redirect]
  deriving Show

data Word = Word String | DollarN Int | DollarDollar
  deriving Show

data Redirect = Redirect OpenMode FD RedirectSource
  deriving Show

data RedirectSource = FromPath Word | FromFD FD
  deriving Show

runScript :: Env -> Script -> Prog ()
runScript env = run where
  run :: Script -> Prog ()
  run = \case
    ShamError s -> err2 s
    Seq s1 s2 -> do run s1; run s2

    IfEq w1 w2 s1 s2 -> do -- TODO: example for this
      x1 <- evalWord env w1
      x2 <- evalWord env w2
      run (if x1 == x2 then s1 else s2)

    Null -> do
      pure ()

    Invoke1 step mode ->
      runStep env mode step
      --runPipeline env mode [step] -- TODO: can it just be this?

    Pipeline steps mode ->
      runPipeline env mode steps


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
  let command = Command ("sham",[])
  spawn1 command (do -- TODO: dont loose the name of the actual pipe element
               dup2 (FD 1) w
               MeNicks.Call Close w
               MeNicks.Call Close r
               prog1
           ) $ \child1 -> do
    spawn1 command (do
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

evalWord :: Env -> Word -> Prog String
evalWord Env{pid,com,args} = \case
  Word s -> pure s
  DollarDollar -> let (Pid n) = pid in pure $ show n
  DollarN n ->
    if n > length args then do err2 ("$" ++ show n ++ " unbound"); pure "" else
      pure $ (com:args)!!n

decode :: String -> Act
decode = \case
  "exit" -> Exit
  "echo" -> Echo
  "exec" -> Exec
  "." -> SourceSham
  name -> RunExternal name

data Act
  = Exit
  | Echo
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
        Nothing -> do err2 "cant find sham interpreter"; MeNicks.Exit
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
      MeNicks.Exit
    Right () -> pure ()
