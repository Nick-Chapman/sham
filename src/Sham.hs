-- | 'sham' is a shell-style command interpreter which runs on MeNicks.
module Sham (sham) where

import Environment (Environment)
import Interaction (Prompt(..),EOF(..))
import Lib (loadFile,stdin,stdout,stderr,close,read,write,exit,withOpen,readAll,execCommand,forkWait,forkNoWait,dup2,shift2)
import Prelude hiding (Word,read)
import Prog
import Syntax (parseLine,Script(..),Word(..),Pred(..),Redirect(..),RedirectSource(..),Var(..))
import qualified Environment
import qualified Path (create)

sham :: Prog ()
sham = do
  Command(_sham,args) <- Prog.Argv
  env <- initEnv
  case args of
    "-c":rest -> do
      let script :: Script = parseLine (unwords rest)
      runScript env script $ \_env -> pure ()
    path:args -> do
      lines <- loadFile path
      let script = parseLines lines
      runScript env { argv = path:args } script $ \_env -> pure ()
    [] ->
      loop env 1

initEnv :: Prog Env
initEnv = do
  pid <- Prog.MyPid
  environment <- Prog.MyEnvironment
  let prefix =
        case Environment.get environment (Var "prefix") of
          Nothing -> "sham"
          Just x -> x ++ "/sham"
  let environment' = Environment.set environment (Var "prefix") prefix
  pure $ Env { pid, argv = ["sham"], environment = environment' }

loop :: Env -> Int -> Prog ()
loop env@Env{environment} n = do
  let prefix = maybe "" id (Environment.get environment (Var "prefix"))
  let prompt = prefix ++ "[" ++ show n ++ "]$ "
  read (Prompt prompt) (FD 0) >>= \case
    Left EOF -> pure ()
    Right line -> do
      let script = parseLine line
      --Trace (show ("sham",script))
      runScript env script $ \env -> loop env (n+1)

parseLines :: [String] -> Script
parseLines lines = foldl QSeq QNull (map parseLine lines)

data Env = Env
  { pid :: Pid
  , argv :: [String]
  , environment :: Environment
  }

-- done means we are in an Exec context and so can 'take-over' the process
data K = Done | Cont (Env -> Prog ())

runK :: Env -> K -> Prog ()
runK env = \case
  Done -> exit -- not pure () !!
  Cont k -> k env

runScript :: Env -> Script -> (Env -> Prog ()) -> Prog ()
runScript env0 scrip0 k = loop env0 scrip0 (Cont $ \env -> k env) where

  loop :: Env -> Script -> K -> Prog ()
  loop env = \case

    QNull -> \k -> runK env k

    QSeq s1 s2 -> \k -> do
      loop env s1 $ Cont $ \env -> loop env s2 k

    QSource w ws -> \k -> do
      com <- evalWord env w
      args <- mapM (evalWord env) ws
      script <- loadShamScript com
      let Env{argv=saved} = env
      runScript env { argv = (com:args) } script $ \env -> runK env {argv = saved } k

    QIf pred s1 s2 -> \k -> do
      b <- evalPred env pred
      loop env (if b then s1 else s2) k

    QShamError mes -> \k -> do
      write stderr mes
      runK env k

    QSetVar x w -> \k -> do
      v <- evalWord env w
      let Env{environment} = env
      runK env { environment = Environment.set environment x v } k

    QReadIntoVar x -> \k -> do
      line <- builtinRead
      let Env{environment} = env
      runK env { environment = Environment.set environment x line } k

    QEcho ws -> \k -> do
      args <- mapM (evalWord env) ws
      builtinEcho args
      runK env k

    QExit -> \_ignored_k -> do
      exit

    QExec s -> \_ignored_k -> do
      loop env s Done

    QInvoke w ws -> \k -> do
      let Env{environment} = env
      com <- evalWord env w
      args <- mapM (evalWord env) ws
      case k of
        Done -> execCommand environment (Command (com,args))
        Cont k -> do
          forkWait $
            execCommand environment (Command (com,args))
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
evalWord Env{pid,argv,environment} = \case
  Word s -> pure s
  DollarDollar -> let (Pid n) = pid in pure $ show n
  DollarHash -> pure $ show (length argv - 1)
  DollarN n ->
    if n >= length argv
    then do write stderr ("$" ++ show n ++ " unbound"); pure ""
    else pure $ argv!!n
  DollarName x ->
    case Environment.get environment x of
      Nothing -> do write stderr ("$" ++ show x ++ " unbound"); pure ""
      Just v -> pure v

builtinEcho :: [String] -> Prog ()
builtinEcho args =
  write stdout (unwords args)

builtinRead :: Prog String
builtinRead =
  read NoPrompt stdin >>= \case
    Left EOF -> exit
    Right line -> return line

loadShamScript :: String -> Prog Script
loadShamScript path = do
  lines <- do
    withOpen (Path.create path) OpenForReading $ \fd -> do
      readAll fd
  pure $ parseLines lines

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
              Just incoming -> shift2 stdin incoming
            prog1
          Just childPid -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> close incoming
            mapM_ Prog.Wait (childPid:pids)

      prog2:progs -> do
        PipeEnds{w=pipeW,r=pipeR} <- Prog.Call SysPipe ()
        Prog.Fork >>= \case
          Nothing -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> shift2 stdin incoming
            close pipeR
            shift2 stdout pipeW
            prog1
          Just childPid -> do
            case incoming of
              Nothing -> pure ()
              Just incoming -> close incoming
            close pipeW
            loop (Just pipeR) (childPid:pids) prog2 progs
