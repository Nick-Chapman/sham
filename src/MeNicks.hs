-- | The 'MeNicks' operating system. Run 'init', which forks a 'sham' interpreter.
module MeNicks (start) where

import Data.Map (Map)
import Environment (Environment)
import FileSystem (FileSystem)
import Interaction (Interaction(..),OutMode(..))
import OpenFiles (OpenFiles)
import Prelude hiding (init)
import Prog
import SysCall (FdEnv,env0,dupEnv,closeEnv,runSys,openFiles)
import qualified Data.Map.Strict as Map
import qualified Environment (empty,set,Var(..))
import qualified Init (init)
import qualified OpenFiles (init)

traceMeNicks :: Bool
traceMeNicks = False

start :: FileSystem -> Interaction
start fs = do
  let oF = OpenFiles.init fs
  let initProc = Proc
        { command = Command ("init",[])
        , environment = environment0
        , fde = env0
        , action = linearize Init.init (\() -> A_Halt)
        }
  let (state,pid) = newPid (initState oF)
  resume pid initProc state

environment0 :: Environment
environment0 =
  Environment.set Environment.empty (Environment.Var "Version") "MeNicks-0.1"

linearize :: Prog a -> (a -> Action) -> Action
linearize p0 = case p0 of
  Ret a -> \k -> k a
  Bind p f -> \k ->linearize p $ \a -> linearize (f a) k
  Exit -> \_ignoredK -> A_Halt
  WriteConsole mode message -> \k -> A_WriteConsole mode message (k ())
  Trace message -> \k -> A_Trace message (k ())
  Fork -> A_Fork
  Exec e command prog -> \_ignoredK -> A_Exec e command (linearize prog $ \_ -> A_Halt)
  Wait pid -> \k -> A_Wait pid (k ())
  Alive pid -> A_Alive pid
  Argv -> A_Argv
  MyPid -> A_MyPid
  MyEnvironment -> A_MyEnvironment
  Procs -> A_Procs
  Lsof -> A_Lsof
  Call sys arg -> A_Call sys arg

data Action where
  A_Halt :: Action
  A_WriteConsole :: OutMode -> String -> Action -> Action
  A_Trace :: String -> Action -> Action
  A_Fork :: (Maybe Pid -> Action) -> Action
  A_Exec :: Environment -> Command -> Action -> Action
  A_Wait :: Pid -> Action -> Action
  A_Alive :: Pid -> (Bool -> Action) -> Action
  A_Argv :: (Command -> Action) -> Action
  A_MyPid :: (Pid -> Action) -> Action
  A_MyEnvironment :: (Environment -> Action) -> Action
  A_Procs :: ([(Pid,Command)] -> Action) -> Action
  A_Lsof :: ([(Pid,Command,FD,OF)] -> Action) -> Action
  A_Call :: (Show a, Show b) => SysCall a b -> a -> (b -> Action) -> Action

resume :: Pid -> Proc -> State -> Interaction
resume me proc0@(Proc{command=command0,environment,fde,action=action0}) state@State{os} =
  case action0 of

  A_Halt -> do
    traceProc (me,proc0) "Halt(before)" $ do
    traceProc (me,proc0) ("fde: " ++ show fde) $ do
    trace (show state) $ do
    let state' = state { os = closeEnv fde os }
    traceProc (me,proc0) "Halt(after)" $ do
    traceProc (me,proc0) ("fde: " ++ show fde) $ do
    trace (show state') $ do
    case choose state' of
      Nothing -> I_Halt
      Just (state',other,proc2) ->
        resume other proc2 state'

  A_Trace message action ->
    I_Trace message (yield me proc0 { fde, action } state)

  A_WriteConsole mode message action ->
    I_Write mode message (yield me proc0 { fde, action } state)

  A_Fork f -> do
    let state' = state { os = dupEnv fde os }
    let (state'',pid) = newPid state'
    traceProc (me,proc0) ("Fork:"++show pid) $ do
    trace (show state'') $ do
    let child = proc0 { action = f Nothing }
    let parent = proc0 { action = f (Just pid) }
    yield me parent (suspend pid child state'')

  A_Exec environment command action -> do
    yield me proc0 { environment, command, action } state

  A_Wait pid action ->
    --traceProc (me,proc0) ("Wait:"++show pid) $ do -- This happens a lot!
    if running pid state
    then block me proc0 state
     -- TODO: resume instead of yield (less rr)
    else yield me proc0 { action } state

  A_Alive pid f -> do
    let answer = running pid state
    yield me proc0 { action = f answer } state

  A_MyPid f -> do yield me proc0 { action = f me } state

  A_MyEnvironment f -> do
    yield me proc0 { action = f environment } state

  A_Argv f -> do yield me proc0 { action = f command0  } state

  A_Procs f -> do
    let res = (me,command0) : allProcs state
    yield me proc0 { action = f res } state

  A_Lsof f -> do
    let res = lsof (me,proc0) state
    yield me proc0 { action = f res } state

  A_Call sys arg f -> do
    case runSys sys os fde arg of
      Left Block ->
        traceProc (me,proc0) (show sys ++ show arg ++" BLOCKED") $ do
        undefined $ -- TODO: outer blocking no longer used or supported; kill
          block me proc0 state
      Right proceed -> do
        proceed $ \os fde res -> do
          traceProc (me,proc0) (show sys ++ show arg ++" --> " ++ show res) $ do
          traceProc (me,proc0) ("fde: " ++ show fde) $ do
          let state' = state { os }
          trace (show state') $ do
          case res of
            Left Block ->
              block me proc0 state'

            Right res -> do
              let action = f res
              yield me proc0 { fde, action } state'


block :: Pid -> Proc -> State -> Interaction
block = yield -- TODO: track blocked Procs

yield :: Pid -> Proc -> State -> Interaction
yield me proc1 state = do
  case choose state of
    Nothing ->
      --trace (show ("yield, only me!", me)) $
      resume me proc1 state -- nothing else to do, so continue
    Just (state,other,proc2) ->
      --trace (show ("yield", me, "-->", other)) $
      resume other proc2 (suspend me proc1 state)

traceProc :: (Pid,Proc) -> String -> Interaction -> Interaction
traceProc (me,Proc{command}) mes =
  trace (show me ++ "(" ++ show command ++ ") " ++ mes)

trace :: String -> Interaction -> Interaction
trace s = if traceMeNicks then I_Trace s else id

data Proc = Proc
  { command :: Command
  , environment :: Environment
  , fde :: FdEnv
  , action :: Action
  }

data State = State
  { os :: OpenFiles
  , nextPid :: Pid
  , waiting :: Map Pid Proc
  , suspended :: Map Pid Proc
  }

initState :: OpenFiles -> State
initState os = State
  { os
  , nextPid = 1
  , waiting = Map.empty
  , suspended = Map.empty
  }

instance Show State where
  show State{os} = show os

allProcs :: State -> [(Pid,Command)]
allProcs State{waiting,suspended} =
  [ (pid,command) | (pid,Proc{command}) <-  Map.toList waiting ++ Map.toList suspended ]

lsof :: (Pid,Proc) -> State -> [(Pid,Command,FD,OF)]
lsof (me,proc0) State{os,waiting,suspended} =
  [ (pid,command,fd,oF)
  | (pid,Proc{command,fde}) <- (me,proc0) : (Map.toList waiting ++ Map.toList suspended)
  , (fd,oF) <- openFiles os fde
  ]

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
