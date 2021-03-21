-- | The AST of a 'sham' script, which can be interpreted as a program.
module Script (
  Script(..), WaitMode(..),
  Step(..), Redirect(..), RedirectSource(..),
  Pred(..),Word(..),Var(..),
  run,
  runScript, Env(..)
  ) where

import Data.Map (Map)
import Interaction (Prompt(..))
import Lib (stdin,stdout,stderr,read,write,exit,withOpen,readAll,execCommand,forkWait,forkNoWait)
import Misc (EOF(..),PipeEnds(..))
import Prelude hiding (Word,read)
import Prog (FD,SysCall(..),BadFileDescriptor(..),Pid(..),Prog,Command(..),OpenMode(..))
import qualified Data.Map.Strict as Map
import qualified Path (create)
import qualified Prog (Prog(..))

run :: ([String] -> Script) -> Script -> Prog ()
run shamParser script = do
  Command(com,args) <- Prog.Argv
  pid <- Prog.MyPid
  let bindings = Map.empty
  let env = Env { pid, com, args, bindings, shamParser }
  runScript env script

data Env = Env
  { com :: String
  , args :: [String]
  , bindings :: Map Var String
  , pid :: Pid
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
    ShamError s -> \_k -> do write stderr s
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
  read NoPrompt stdin >>= \case
    Left EOF -> exit
    Right line -> return line


runPipeline :: Env -> WaitMode -> [Step] -> Prog ()
runPipeline env wm = \case
    [] -> undefined
    step1:steps -> loop Nothing [] step1 steps
  where
    loop :: Maybe FD -> [Pid] -> Step -> [Step] -> Prog ()
    loop incoming pids step1 = \case
      [] -> do
        Prog.Fork >>= \case

          Nothing -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> do dup2 stdin incoming; Prog.Call Close incoming
            runStepAsPipeStage env step1

          Just childPid -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> Prog.Call Close incoming
            case wm of
              Wait -> mapM_ Prog.Wait (childPid:pids)
              NoWait -> pure ()

      step2:steps -> do
        PipeEnds{w=pipeW,r=pipeR} <- Prog.Call SysPipe ()
        Prog.Fork >>= \case

          Nothing -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> do dup2 stdin incoming; Prog.Call Close incoming
            Prog.Call Close pipeR
            dup2 stdout pipeW; Prog.Call Close pipeW
            runStepAsPipeStage env step1

          Just childPid -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> Prog.Call Close incoming
            Prog.Call Close pipeW
            loop (Just pipeR) (childPid:pids) step2 steps


runStepAsPipeStage :: Env -> Step -> Prog ()
runStepAsPipeStage env  = \case
  SubShell script rs ->
    error "broken" $ subshell env (runScript env script) (rs,Wait)
  Run w1 ws rs -> do
    com <- evalWord env w1
    args <- mapM (evalWord env) ws
    mapM_ (execRedirect env) rs
    execStage args (decode com)

execStage :: [String] -> Act -> Prog ()
execStage args = \case
  Exit -> exit
  Echo ->
    execCommand (Command ("echo",args))
  SourceSham ->
    undefined -- TODO: do what in a pipeline?
  RunExternal name -> do
    execCommand (Command (name,args))
  Exec -> do
    case args of
      [] -> exit
      name:args -> do
        execCommand (Command (name,args))

runStep :: Env -> WaitMode -> Step -> Prog ()
runStep env mode = \case
  SubShell script rs ->
    subshell env (runScript env script) (rs,mode)
  Run w ws rs -> do
    com <- evalWord env w
    args <- mapM (evalWord env) ws
    runAct env (rs,mode) args (decode com)

subshell :: Env -> Prog () -> ([Redirect], WaitMode) -> Prog ()
subshell env prog = \case
  -- we can skip the subshell if there are no redircts and we must wait
  ([],Wait) -> prog
  (rs,wm) -> do -- otherwise we must fork
    forkMode wm $ do
      mapM_ (execRedirect env) rs
      prog
      exit

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
      write stderr ("$" ++ show n ++ " unbound")
      pure ""
    else pure $ (com:args)!!n
  DollarName x ->
    case Map.lookup x bindings of
      Nothing -> do
        write stderr ("$" ++ show x ++ " unbound")
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
      ([],Wait,[]) -> Prog.Exit
      _ -> write stderr "exit takes no args, redirects, or (&)"

  Echo ->
    case (rs,wm) of
      -- TODO: can we do builtin echo even with redirects?
      ([],Wait) -> write stdout (unwords args) --builtin echo
      _ -> do
        forkMode wm $ do
          mapM_ (execRedirect env) rs
          execCommand (Command ("echo",args))

  SourceSham -> do
    case (rs,wm,args) of
      ([],Wait,com:args) -> do
        script <- loadShamScript env com
        runScript env { args } script
      _ -> write stderr "source takes at least one argument, but no redirects or (&)"

  RunExternal name -> do
    forkMode wm $ do
      mapM_ (execRedirect env) rs
      execCommand (Command (name,args))

  Exec -> do
    case wm of
      NoWait -> write stderr "exec may not be run (&)"
      Wait -> do
        mapM_ (execRedirect env) rs
        case args of
          [] -> pure () -- we just do redirects and nothing else
          name:args ->
            execCommand (Command (name,args))

loadShamScript :: Env -> String -> Prog Script
loadShamScript env path = do
  lines <- do
    withOpen (Path.create path) OpenForReading $ \fd -> do
      readAll fd
  pure $ shamParser env lines

forkMode :: WaitMode -> Prog () -> Prog ()
forkMode = \case
  Wait -> forkWait
  NoWait -> forkNoWait

execRedirect :: Env -> Redirect -> Prog ()
execRedirect env = \case
  Redirect mode dest (FromPath path) -> do
    path <- evalWord env path
    withOpen (Path.create path) mode $ \src -> do
      dup2 dest src
  Redirect _mode dest (FromFD src) -> do -- do we care what the mode is?
    dup2 dest src

dup2 :: FD -> FD -> Prog ()
dup2 d s = do
  Prog.Call Dup2 (d,s) >>= \case
    Left BadFileDescriptor -> do
      write stderr $ "bad file descriptor: " ++ show s
      exit
    Right () -> pure ()
