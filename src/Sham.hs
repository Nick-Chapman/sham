-- | 'sham' is a shell-style command interpreter which runs on MeNicks.
module Sham (sham) where

import Data.Map (Map)
import Interaction (Prompt(..))
import Lib (loadFile,stdin,stdout,stderr,read,write,exit,withOpen,readAll,execCommand,forkWait,forkNoWait)
import Misc (EOF(..),PipeEnds(..))
import Prelude hiding (Word,read)
import Prog (Prog,FD(..),Command(..),OpenMode(..),SysCall(..),BadFileDescriptor(..),Pid(..))
import Syntax (parseLine,Script(..),Word(..),Pred(..),Redirect(..),RedirectSource(..),Var(..))
import qualified Data.Map.Strict as Map
import qualified Path (create)
import qualified Prog (Prog(Argv,MyPid,Fork,Call,Wait))

sham :: Prog ()
sham = do
  Command(_sham,args) <- Prog.Argv
  case args of

    "-c":rest -> do
      let script :: Script = parseLine (unwords rest)
      pid <- Prog.MyPid
      let com = "sham"
      let args = []
      let bindings = Map.empty
      let env = Env { pid, com, args, bindings, shamParser = parseLines}
      runScript env script

    path:args -> do
      lines <- loadFile path
      let script = parseLines lines
      pid <- Prog.MyPid
      let com = path
      let bindings = Map.empty
      let env = Env { pid, com, args, bindings, shamParser = parseLines}
      runScript env script

    [] -> loop 1 where
      loop :: Int -> Prog ()
      loop n = do
        let level :: Int = 1 -- TODO: get from env
        let prompt = "sham[" ++ show level ++ "." ++ show n ++ "]$ "
        read (Prompt prompt) (FD 0) >>= \case
          Left EOF -> pure ()
          Right line -> do
            let script :: Script = parseLine line
            pid <- Prog.MyPid
            let com = "sham"
            let args = []
            let bindings = Map.empty
            let env = Env { pid, com, args, bindings, shamParser = parseLines}
            runScript env script
            loop (n+1)


parseLines :: [String] -> Script
parseLines lines = foldl QSeq QNull (map parseLine lines)


data Env = Env
  { com :: String
  , args :: [String]
  , bindings :: Map Var String
  , pid :: Pid
  , shamParser :: [String] -> Script
  }

-- done means we are in an Exec context and so can 'take-over' the process
data K = Done | Cont (Env -> Prog ())

runK :: Env -> K -> Prog ()
runK env = \case
  Done -> exit -- not pure () !!
  Cont k -> k env

runScript :: Env -> Script -> Prog ()
runScript env0 scrip0 = loop env0 scrip0 (Cont $ \_ -> pure ()) where

  loop :: Env -> Script -> K -> Prog ()
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
      pure "" -- TODO: prefer to exit, but mustn't kill sham console
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


