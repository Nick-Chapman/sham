
module Prog ( -- with support for multi-threading!
  Prog(..), Pid,
  SysCall(..),
  OpenMode(..),WriteOpenMode(..),NoSuchPath(..),FD(..),
  run,
  ) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import FileSystem (FileSystem,NoSuchPath(..))
import Interaction (Interaction(..))
import Misc (Block(..))
import OsState (OsState,OpenMode(..),WriteOpenMode(..))
import SysCall (SysCall(..),Env,env0,dupEnv,closeEnv,runSys,FD(..))
import qualified Data.Map.Strict as Map
import qualified OsState (init)

instance Functor Prog where fmap = liftM
instance Applicative Prog where pure = return; (<*>) = ap
instance Monad Prog where return = Ret; (>>=) = Bind

data Prog a where
  Ret :: a -> Prog a
  Bind :: Prog a -> (a -> Prog b) -> Prog b
  Exit :: Prog a
  Trace :: String -> Prog ()
  Fork :: Prog (Maybe Pid)
  Exec :: String -> Prog a -> Prog b
  Wait :: Pid -> Prog ()
  Procs :: Prog [(Pid,Proc)]
  Call :: (Show a,Show b) => SysCall a b -> a -> Prog b

run :: FileSystem -> Prog () -> Interaction
run fs prog = do
  let action = linearize prog (\() -> A_Halt)
  let (state,pid) = newPid (initState fs)
  resume pid (Proc "init" env0 action) state

linearize :: Prog a -> (a -> Action) -> Action
linearize p0 = case p0 of
  Ret a -> \k -> k a
  Bind p f -> \k ->linearize p $ \a -> linearize (f a) k
  Exit -> \_ignoredK -> A_Halt
  Trace message -> \k -> A_Trace message (k ())
  Fork -> A_Fork
  Exec commandString prog -> \_ignoredK -> A_Exec commandString (linearize prog $ \_ -> A_Halt)
  Wait pid -> \k -> A_Wait pid (k ())
  Procs -> A_Procs
  Call sys arg -> A_Call sys arg

data Action where
  A_Halt :: Action
  A_Trace :: String -> Action -> Action
  A_Fork :: (Maybe Pid -> Action) -> Action
  A_Exec :: String -> Action -> Action
  A_Wait :: Pid -> Action -> Action
  A_Procs :: ([(Pid,Proc)] -> Action) -> Action
  A_Call :: (Show a, Show b) => SysCall a b -> a -> (b -> Action) -> Action

instance Show Action where
  show = \case
    A_Halt{} -> "Halt"
    A_Trace{} -> "Trace"
    A_Fork{} -> "Fork"
    A_Exec{} -> "Exec"
    A_Wait{} -> "Wait"
    A_Procs{} -> "Procs"
    A_Call{} -> "Call"

resume :: Pid -> Proc -> State -> Interaction
resume me proc0@(Proc{env,action=action0}) state@State{os} =
 --I_Trace (show ("resume",me,action0)) $
  case action0 of

  A_Halt -> do
    --trace me "Halt" $ do
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
    --_trace me ("Fork:"++show pid) $ do
    let child = proc0 { action = f Nothing }
    let parent = proc0 { action = f (Just pid) }
    yield me parent (suspend pid child state'')

  A_Exec commandString action -> do
    yield me proc0 { commandString, action } state

  A_Wait pid action ->
    --trace me ("Wait:"++show pid) $ do -- This happens a lot!
    if running pid state
    then block me proc0 state
     -- TODO: resume instead of yield (less rr)
    else yield me proc0 { action } state

  A_Procs f -> do
    let res = (me,proc0) : allProcs state
    --trace me ("Procs()="++show res) $ do
    let action = f res
    yield me proc0 { action } state

  A_Call sys arg f -> do
    -- TODO: allow trace tobe turned on/off interactively at the console
    --trace me (show sys ++ show arg ++"...") $ do
    case runSys sys os env arg of
      Left Block ->
        --trace me (show sys ++ show arg ++" BLOCKED") $ do
        block me proc0 state
      Right proceed -> do
        proceed $ \os env res -> do
          --trace me (show sys ++ show arg ++" --> " ++ show res) $ do
          --trace me ("env: " ++ show env) $ do
          let state' = state { os }
          --I_Trace (show state') $ do
          let action = f res
          yield me proc0 { env, action } state'

_trace :: Pid -> String -> Interaction -> Interaction
_trace me mes = I_Trace (show me ++ " " ++ mes)

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

data Proc = Proc { commandString :: String, env :: Env, action :: Action }

instance Show Proc where
  show Proc{commandString} = commandString

newtype Pid = Pid Int deriving (Eq,Ord,Num)
instance Show Pid where show (Pid n) = "[" ++ show n ++ "]"

data State = State
  { os :: OsState
  , nextPid :: Pid
  , waiting :: Map Pid Proc
  , suspended :: Map Pid Proc
  }

instance Show State where
  show State{os} = show os

allProcs :: State -> [(Pid,Proc)]
allProcs State{waiting,suspended} = Map.toList waiting ++ Map.toList suspended

initState :: FileSystem -> State
initState fs = State
  { os = OsState.init fs
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
