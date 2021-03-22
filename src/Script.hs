-- | The AST of a 'sham' script, which can be interpreted as a program.
module Script (
  Script(..), WaitMode(..),
  Step(..), Redirect(..), RedirectSource(..),
  Invocation(..),
  Pred(..),Word(..),Var(..),
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
runScript env script = do
  let s = conv script
  --Prog.Trace (show s)
  runScrip env s

conv :: Script -> Scrip
conv = fromScript where
  fromScript :: Script -> Scrip
  fromScript = \case
    ShamError s -> QShamError s
    Null -> QNull
    Seq s1 s2 -> QSeq (fromScript s1) (fromScript s2)
    If pred s1 s2 -> QIf pred (fromScript s1) (fromScript s2)
    ReadIntoVar x -> QReadIntoVar x
    Invoke1 step Wait -> fromStep step
    Invoke1 step NoWait -> QBackGrounding (fromStep step)
    Pipeline steps Wait -> QPipeline (map fromStep steps)
    Pipeline steps NoWait -> QBackGrounding (QPipeline (map fromStep steps))

  fromStep :: Step -> Scrip
  fromStep = \case
    XSubShell script rs -> QRedirecting (fromScript script) rs
    Run (Invocation (Word "exec") []) rs ->
      QExec (QRedirecting QNull rs)
    Run (Invocation (Word "exec") (w:ws)) rs ->
      QExec (QRedirecting (fromInvocation (Invocation w ws)) rs)
    Run invocation rs -> QRedirecting (fromInvocation invocation) rs

  fromInvocation :: Invocation -> Scrip
  fromInvocation = \case
    -- sort this out at parse time!
    Invocation (Word ".") (w:ws) -> QSource w ws
    Invocation (Word ".") [] -> QShamError "source takes at least one argument"
    Invocation (Word "echo") ws -> QEcho ws
    Invocation (Word "exit") [] -> QExit
    Invocation (Word "exit") _ -> QShamError "exit takes no args"
    --Invocation (Word "exec") [] -> QExec QNull
    --Invocation (Word "exec") (w:ws) -> QExec (QInvoke w ws)
    Invocation w ws -> QInvoke w ws

data Scrip -- no "t" for now. TODO: regain "t" and loose Q prefix when conv step is avoided
  = QNull
  | QSeq Scrip Scrip
  | QIf Pred Scrip Scrip
  | QShamError String
  | QEcho [Word]
  | QReadIntoVar Var
  | QExit
  | QExec Scrip
  | QInvoke Word [Word]
  | QSource Word [Word]
  | QPipeline [Scrip]
  | QBackGrounding Scrip
  | QRedirecting Scrip [Redirect] -- TODO: have just 1 redirect!
  deriving Show

-- done means we are in an Exec context and so can 'take-over' the process
data K = Done | Cont (Env -> Prog ())

runK :: Env -> K -> Prog ()
runK env = \case
  Done -> exit -- not pure () !!
  Cont k -> k env

runScrip :: Env -> Scrip -> Prog ()
runScrip env0 scrip0 = loop env0 scrip0 (Cont $ \_ -> pure ()) where

  loop :: Env -> Scrip -> K -> Prog ()
  loop env = \case
    QNull -> \k -> runK env k

    QSeq s1 s2 -> \k -> do
      loop env s1 $ Cont $ \env -> loop env s2 k

    QSource w ws -> \k -> do
      com <- evalWord env w
      args <- mapM (evalWord env) ws
      script <- loadShamScript env com
      runScript env { args } script
      runK env k

    QIf pred s1 s2 -> \k -> do
      b <- evalPred env pred
      loop env (if b then s1 else s2) k

    QShamError mes -> \_ignored_k -> do
      write stderr mes

    QReadIntoVar x -> \k -> do
      line <- builtinRead
      let Env{bindings} = env
      runK env { bindings = Map.insert x line bindings } k

    QEcho ws -> \k -> do
      args <- mapM (evalWord env) ws
      builtinEcho args
      runK env k

    QExit -> \_ignored_k -> do
      exit

    QExec s -> \_ignored_k -> do
      loop env s Done

    QInvoke w ws -> \k -> do
      com <- evalWord env w
      args <- mapM (evalWord env) ws
      case k of
        Done -> execCommand (Command (com,args))
        Cont k -> do
          forkWait $
            execCommand (Command (com,args))
          k env

    QRedirecting s [] -> \k -> loop env s k

    QRedirecting s rs -> \k -> do
      case k of
        Done -> do
          mapM_ (execRedirect env) rs
          loop env s Done
        Cont k -> do
          forkWait $ do
            mapM_ (execRedirect env) rs
            loop env s Done
          k env

    QPipeline scrips -> \k -> do
      pipeline (map (\s -> loop env s Done) scrips)
      runK env k

    QBackGrounding s -> \k -> do
      forkNoWait $ do
        loop env s Done
      runK env k


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

builtinEcho :: [String] -> Prog ()
builtinEcho args =
  write stdout (unwords args)

builtinRead :: Prog String
builtinRead =
  read NoPrompt stdin >>= \case
    Left EOF -> exit
    Right line -> return line

loadShamScript :: Env -> String -> Prog Script
loadShamScript env path = do
  lines <- do
    withOpen (Path.create path) OpenForReading $ \fd -> do
      readAll fd
  pure $ shamParser env lines

execRedirect :: Env -> Redirect -> Prog ()
execRedirect env = \case
  Redirect om dest (FromPath path) -> do
    path <- evalWord env path
    withOpen (Path.create path) om $ \src -> do
      dup2 dest src
  Redirect _om dest (FromFD src) -> do -- do we care what the open-mode is?
    dup2 dest src

pipeline :: [Prog()] -> Prog ()
pipeline = \case
    [] -> error "runPipeline[]"
    prog1:progs -> loop Nothing [] prog1 progs
  where
    loop :: Maybe FD -> [Pid] -> Prog () -> [Prog ()] -> Prog ()
    loop incoming pids prog1 = \case
      [] -> do
        Prog.Fork >>= \case
          Nothing -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> do dup2 stdin incoming; Prog.Call Close incoming
            prog1
          Just childPid -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> Prog.Call Close incoming
            mapM_ Prog.Wait (childPid:pids)

      prog2:progs -> do
        PipeEnds{w=pipeW,r=pipeR} <- Prog.Call SysPipe ()
        Prog.Fork >>= \case
          Nothing -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> do dup2 stdin incoming; Prog.Call Close incoming
            Prog.Call Close pipeR
            dup2 stdout pipeW; Prog.Call Close pipeW
            prog1
          Just childPid -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> Prog.Call Close incoming
            Prog.Call Close pipeW
            loop (Just pipeR) (childPid:pids) prog2 progs

dup2 :: FD -> FD -> Prog ()
dup2 d s = do
  Prog.Call Dup2 (d,s) >>= \case
    Left BadFileDescriptor -> do
      write stderr $ "bad file descriptor: " ++ show s
      exit
    Right () -> pure ()


