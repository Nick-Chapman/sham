
module Os2 ( -- with support for co-operatove threading!
  Prog(..),
  SysCall(..),
  OpenMode(..),NoSuchPath(..),FD(..),
  sim,
  ) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import FileSystem (FileSystem,NoSuchPath(..))
import Interaction (Interaction(..))
import Misc (Block(..))
import OsState (OsState,OpenMode(..))
import SysCall (SysCall(..),Env,env0,runSys,FD(..))
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
  Spawn :: Prog () -> (Pid -> Prog a) -> Prog a
  Wait :: Pid -> Prog ()
  Call :: (Show a,Show b) => SysCall a b -> a -> Prog b

sim :: FileSystem -> Prog () -> Interaction
sim fs prog = do
  let action = linearize prog (\() -> A_Halt)
  let (state,pid) = newPid (initState fs)
  resume pid (Proc env0 action) state

linearize :: Prog a -> (a -> Action) -> Action
linearize p0 = case p0 of
  Ret a -> \k -> k a
  Bind p f -> \k ->linearize p $ \a -> linearize (f a) k
  Exit -> \_ignoredK -> A_Halt
  Trace message -> \k -> A_Trace message (k ())
  Spawn child f -> \k -> do
    let action = linearize child $ \() -> A_Halt
    A_Spawn action $ \pid -> linearize (f pid) k
  Wait pid -> \k -> A_Wait pid (k ())
  Call sys arg -> \k -> A_Call sys arg k

data Action where
  A_Halt :: Action
  A_Trace :: String -> Action -> Action
  A_Spawn :: Action -> (Pid -> Action) -> Action
  A_Wait :: Pid -> Action -> Action
  A_Call :: (Show a, Show b) => SysCall a b -> a -> (b -> Action) -> Action

----------------------------------------------------------------------

resume :: Pid -> Proc -> State -> Interaction
resume me proc0@(Proc env action0) state@State{os} = case action0 of

  A_Halt -> do
    case choose state of
      Nothing -> I_Halt
      Just (state,other,proc2) ->
        resume other proc2 state

  A_Trace message action ->
    I_Trace message (resume me (Proc env action) state)

  A_Spawn action f -> do
    -- TODO: dup all file-descriptors in env
    let child = Proc env action
    let (state',pid) = newPid state
    let parent = Proc env (f pid)
    yield me parent (suspend pid child state')

  A_Wait pid action ->
    if running pid state
    then block me proc0 state
    else yield me (Proc env action) state

  A_Call sys arg f -> do
    --I_Trace (show ("Call",sys,arg)) $ do
    case runSys sys os env arg of
      Left Block ->
        --I_Trace (show ("Call/blocked",sys,arg)) $ do
        block me proc0 state
      Right proceed -> do
        proceed $ \os env res -> do
          --I_Trace (show ("Call/proceed",sys,arg,res)) $ do
          let action = f res
          yield me (Proc env action) state { os }

block :: Pid -> Proc -> State -> Interaction
block = yield

yield :: Pid -> Proc -> State -> Interaction
yield me proc1 state = do
  case choose state of
    Nothing ->
      resume me proc1 state -- nothing else to do, so continue
    Just (state,other,proc2) ->
      --I_Trace (show ("yield",me,other)) $ do
      resume other proc2 (suspend me proc1 state)

----------------------------------------------------------------------

data Proc = Proc Env Action

newtype Pid = Pid Int deriving (Eq,Ord,Num,Show)

data State = State
  { os :: OsState
  , nextPid :: Pid
  , waiting :: Map Pid Proc
  , suspended :: Map Pid Proc
  }

initState :: FileSystem -> State
initState fs = State
  { os = OsState.init fs
  , nextPid = 1000
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
