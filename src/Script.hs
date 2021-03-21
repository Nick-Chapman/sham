-- | The AST of a 'sham' script, which can be interpreted as a program.
module Script (
  Script(..), WaitMode(..),
  Step(..), Redirect(..), RedirectSource(..),
  Invocation(..),
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
  = Run Invocation [Redirect]
  | XSubShell Script [Redirect]
  deriving Show

data Invocation
  = Invocation Word [Word]
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
runScript = runScriptM (X [])

runScriptM :: Mode -> Env -> Script -> Prog ()
runScriptM _mode env0 script0 = loop env0 script0 (\_ -> pure ()) where

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

    Invoke1 step wm -> \k -> do
      _runStep env wm step
      -- _doStep (addWait mode wm) env step
      k env

    Pipeline steps wm -> \k -> do
      runPipeline env wm steps
      k env

builtinRead :: Prog String
builtinRead =
  read NoPrompt stdin >>= \case
    Left EOF -> exit
    Right line -> return line


runPipeline :: Env -> WaitMode -> [Step] -> Prog ()
runPipeline env wm = \case
    [] -> error "runPipeline[]"
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
            execStep env step1

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
            execStep env step1

          Just childPid -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> Prog.Call Close incoming
            Prog.Call Close pipeW
            loop (Just pipeR) (childPid:pids) step2 steps


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


-- "exec" means we run this thing, and dont continue afterwards!
-- "run" means we run this thing, and continue

execStep :: Env -> Step -> Prog ()
execStep env  = \case
  XSubShell script rs ->
    --error "broken" $
    --maybeSubshell env (runScript env script) (rs,Wait)
    undefined script rs
  Run invocation rs -> do
    doInvocation (X rs) env invocation

_runStep :: Env -> WaitMode -> Step -> Prog ()
_runStep env wm = \case
  XSubShell script rs -> do
    let mode = undefined
    maybeSubshell env (runScriptM mode env script) (rs,wm)
  Run invocation rs -> do
    doInvocation (R rs wm) env invocation

_doStep :: Mode -> Env -> Step -> Prog ()
_doStep mode env = \case
  XSubShell script rs -> do
    --maybeSubshell env (runScriptM mode env script) (rs,wm)
    forkWait (runScriptM (addRs mode rs) env script)
  Run invocation rs -> do
    --doInvocation (R rs wm) env invocation
    -- TODO: add rs into mode!
    doInvocation (addRs mode rs) env invocation

addRs :: Mode -> [Redirect] -> Mode
addRs mode rs2 = case mode of
  X rs -> X (rs ++ rs2)
  R rs wm -> R (rs ++ rs2) wm

_addWait :: Mode -> WaitMode -> Mode
_addWait mode wm = case mode of
  X rs -> R rs wm
  R rs _ -> R rs wm

doInvocation :: Mode -> Env -> Invocation -> Prog ()
doInvocation mode env = \case
  Invocation w ws -> do
    com <- evalWord env w
    args <- mapM (evalWord env) ws
    doStage mode env args (decode com)

data Mode = X [Redirect] | R [Redirect] WaitMode

doStage :: Mode -> Env -> [String] -> Act -> Prog ()
doStage mode env args = \case
  Exit ->
    case args of
      [] -> exit
      _ -> write stderr "exit takes no args"
  Echo ->
    case mode of
      X rs -> redirecting env rs (execEcho args)
      R rs wm  -> maybeSubshell env (runEcho args) (rs,wm)

  SourceSham ->
    case args of
      com:args -> do
        script <- loadShamScript env com
        runScript env { args } script -- TODO: mode?
      _ ->
        write stderr "source takes at least one argument"

  RunExternal name -> do
    case mode of
      X rs ->
        redirecting env rs $ execCommand (Command (name,args))
      R rs wm ->
        subshell env (rs,wm) (execCommand (Command (name,args)))

  Exec -> do
    (case mode of X rs -> redirecting env rs; R rs _ -> redirecting env rs)
      $ case args of
          [] -> pure ()
          name:args ->
            execCommand (Command (name,args))


maybeSubshell :: Env -> Prog () -> ([Redirect],WaitMode) -> Prog ()
maybeSubshell env prog = \case
  -- we must skip the subshell if there are no redircts and we wait
  ([],Wait) -> prog
  (rs,wm) -> do -- otherwise we must fork
    subshell env (rs,wm) prog

subshell :: Env -> ([Redirect],WaitMode) -> Prog () -> Prog ()
subshell env (rs,wm) prog = do
  forkMode wm $ do
    redirecting env rs (do prog; exit)


forkMode :: WaitMode -> Prog () -> Prog ()
forkMode = \case
  Wait -> forkWait
  NoWait -> forkNoWait

redirecting :: Env -> [Redirect] -> Prog () -> Prog ()
redirecting env rs prog = do
  mapM_ (execRedirect env) rs
  prog


runEcho :: [String] -> Prog ()
runEcho args =
  write stdout (unwords args) --builtin echo

execEcho :: [String] -> Prog ()
execEcho args = do
  runEcho args
  exit




loadShamScript :: Env -> String -> Prog Script
loadShamScript env path = do
  lines <- do
    withOpen (Path.create path) OpenForReading $ \fd -> do
      readAll fd
  pure $ shamParser env lines


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

execRedirect :: Env -> Redirect -> Prog ()
execRedirect env = \case
  Redirect om dest (FromPath path) -> do
    path <- evalWord env path
    withOpen (Path.create path) om $ \src -> do
      dup2 dest src
  Redirect _om dest (FromFD src) -> do -- do we care what the open-mode is?
    dup2 dest src

dup2 :: FD -> FD -> Prog ()
dup2 d s = do
  Prog.Call Dup2 (d,s) >>= \case
    Left BadFileDescriptor -> do
      write stderr $ "bad file descriptor: " ++ show s
      exit
    Right () -> pure ()
