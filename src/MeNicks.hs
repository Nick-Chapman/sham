-- | The 'MeNicks' operating system. Run 'init', which forks a 'sham' interpreter.
module MeNicks (start) where

import Data.Map (Map)
import FileSystem (FileSystem)
import Interaction (Interaction(..))
import Lib (forkWait,tryLoadBinary,write,stderr,exit)
import Misc (Block(..))
import OpenFiles (OpenFiles)
import Prelude hiding (init)
import Prog (Prog(..),Pid(..),Command(..),SysCall,FD,OF)
import SysCall (Env,env0,dupEnv,closeEnv,runSys,openFiles)
import qualified Data.Map.Strict as Map
import qualified OpenFiles (init)

start :: FileSystem -> Interaction
start fs = do
  let initProc = Proc (Command ("init",[])) env0 (linearize init (\() -> A_Halt))
  let (state,pid) = newPid (initState fs)
  resume pid initProc state

init :: Prog ()
init = tryLoadBinary "sham" >>= \case
  Nothing -> do write stderr "init : cannot find sham interpreter"; exit
  Just prog -> do
    forkWait (Exec (Command ("sham",[])) prog)

linearize :: Prog a -> (a -> Action) -> Action
linearize p0 = case p0 of
  Ret a -> \k -> k a
  Bind p f -> \k ->linearize p $ \a -> linearize (f a) k
  Exit -> \_ignoredK -> A_Halt
  Trace message -> \k -> A_Trace message (k ())
  Fork -> A_Fork
  Exec command prog -> \_ignoredK -> A_Exec command (linearize prog $ \_ -> A_Halt)
  Wait pid -> \k -> A_Wait pid (k ())
  Argv -> A_Argv
  MyPid -> A_MyPid
  Procs -> A_Procs
  Lsof -> A_Lsof
  Call sys arg -> A_Call sys arg

data Action where
  A_Halt :: Action
  A_Trace :: String -> Action -> Action
  A_Fork :: (Maybe Pid -> Action) -> Action
  A_Exec :: Command -> Action -> Action
  A_Wait :: Pid -> Action -> Action
  A_Argv :: (Command -> Action) -> Action
  A_MyPid :: (Pid -> Action) -> Action
  A_Procs :: ([(Pid,Command)] -> Action) -> Action
  A_Lsof :: ([(Pid,Command,FD,OF)] -> Action) -> Action
  A_Call :: (Show a) => SysCall a b -> a -> (b -> Action) -> Action

resume :: Pid -> Proc -> State -> Interaction
resume me proc0@(Proc{command=command0,env,action=action0}) state@State{os} =
  case action0 of

  A_Halt -> do
    trace (me,proc0) "Halt" $ do
    let state' = state { os = closeEnv env os }
    --I_Trace (show state') $ do
    case choose state' of
      Nothing -> I_Halt
      Just (state',other,proc2) ->
        resume other proc2 state'

  A_Trace message action ->
    I_Trace message (resume me proc0 { env, action } state)

  A_Fork f -> do
    let state' = state { os = dupEnv env os }
    let (state'',pid) = newPid state'
    trace (me,proc0) ("Fork:"++show pid) $ do
    let child = proc0 { action = f Nothing }
    let parent = proc0 { action = f (Just pid) }
    yield me parent (suspend pid child state'')

  A_Exec command action -> do
    yield me proc0 { command, action } state

  A_Wait pid action ->
    --trace (me,proc0) ("Wait:"++show pid) $ do -- This happens a lot!
    if running pid state
    then block me proc0 state
     -- TODO: resume instead of yield (less rr)
    else yield me proc0 { action } state

  A_MyPid f -> do yield me proc0 { action = f me } state

  A_Argv f -> do yield me proc0 { action = f command0  } state

  A_Procs f -> do
    let res = (me,command0) : allProcs state
    yield me proc0 { action = f res } state

  A_Lsof f -> do
    let res = lsof (me,proc0) state
    yield me proc0 { action = f res } state

  A_Call sys arg f -> do
    trace (me,proc0) (show sys ++ show arg ++"...") $ do
    case runSys sys os env arg of
      Left Block ->
        trace (me,proc0) (show sys ++ show arg ++" BLOCKED") $ do
        block me proc0 state
      Right proceed -> do
        proceed $ \os env res -> do
          -- TODO: Cant show res because of LoadBinary syscall
          --trace (me,proc0) (show sys ++ show arg ++" --> " ++ show res) $ do
          --trace me ("env: " ++ show env) $ do
          let state' = state { os }
          --I_Trace (show state') $ do
          let action = f res
          yield me proc0 { env, action } state'

trace :: (Pid,Proc) -> String -> Interaction -> Interaction
trace (me,Proc{command}) mes =
  -- TODO: allow trace tobe turned on/off interactively at the console
  if True then id else
    I_Trace (show me ++ "(" ++ show command ++ ") " ++ mes)

block :: Pid -> Proc -> State -> Interaction
block = yield -- TODO: track blocked Procs

yield :: Pid -> Proc -> State -> Interaction
yield me proc1 state = do
  case choose state of
    Nothing ->
      --I_Trace (show ("yield, only me!", me)) $
      resume me proc1 state -- nothing else to do, so continue
    Just (state,other,proc2) ->
      --I_Trace (show ("yield", me, "-->", other)) $
      resume other proc2 (suspend me proc1 state)

data Proc = Proc { command :: Command, env :: Env, action :: Action }

data State = State
  { os :: OpenFiles
  , nextPid :: Pid
  , waiting :: Map Pid Proc
  , suspended :: Map Pid Proc
  }

instance Show State where
  show State{os} = show os

allProcs :: State -> [(Pid,Command)]
allProcs State{waiting,suspended} =
  [ (pid,command) | (pid,Proc{command}) <-  Map.toList waiting ++ Map.toList suspended ]

lsof :: (Pid,Proc) -> State -> [(Pid,Command,FD,OF)]
lsof (me,proc0) State{os,waiting,suspended} =
  [ (pid,command,fd,oF)
  | (pid,Proc{command,env}) <- (me,proc0) : (Map.toList waiting ++ Map.toList suspended)
  , (fd,oF) <- openFiles os env
  ]

initState :: FileSystem -> State
initState fs = State
  { os = OpenFiles.init fs
  , nextPid = 1
  , waiting = Map.empty
  , suspended = Map.empty
  }

running :: Pid -> State -> Bool
running pid State{waiting,suspended} =
  Map.member pid waiting || Map.member pid suspended

newPid :: State -> (State,Pid)
newPid s@State{nextPid} = (s { nextPid = nextPid + 1 }, nextPid)

suspend :: Pid -> Proc -> State -> State
suspend pid1 proc1 s@State{suspended} =
  s { suspended = Map.insert pid1 proc1 suspended }

choose :: State -> Maybe (State,Pid,Proc)
choose s@State{waiting,suspended} =
  case Map.minViewWithKey waiting of
    Just ((pid1,proc1),waiting) -> Just (s { waiting }, pid1, proc1)
    Nothing ->
      case Map.minViewWithKey suspended of
        Just ((pid1,proc1),suspended) ->
          -- re-animate the suspended...
          Just (s { waiting = suspended, suspended = Map.empty }, pid1, proc1)
        Nothing ->
          Nothing
