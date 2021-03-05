
module Os2 ( -- with support for co-operatove threading!
  FD(..),
  Prog(..),
  OpenMode(..),NoSuchPath(..),
  sim,
  Interaction(..)
  ) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import FileSystem (FileSystem,NoSuchPath(..))
import Misc (Block(..),EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import OsState (OpenMode(..))
import Path (Path)
import qualified Data.Map.Strict as Map
import qualified File (create)
import qualified FileSystem (create)
import qualified OsState (State,init,ls,open,Key,close,dup,read,write)
import qualified Path (create)

instance Functor Prog where fmap = liftM
instance Applicative Prog where pure = return; (<*>) = ap
instance Monad Prog where return = Ret; (>>=) = Bind

data Prog a where
  Ret :: a -> Prog a
  Bind :: Prog a -> (a -> Prog b) -> Prog b
  Trace :: String -> Prog ()
  --Exit :: Prog a
  Spawn :: Prog () -> (Pid -> Prog a) -> Prog a
  Wait :: Pid -> Prog ()
  Call :: SysCall a b -> a -> Prog b -- TODO: use Call, in place of next 6

  Open :: Path -> OpenMode -> Prog (Either NoSuchPath FD)
  Close :: FD -> Prog ()
  Dup2 :: FD -> FD -> Prog ()
  Read :: FD -> Prog (Either NotReadable (Either EOF String))
  Write :: FD -> String -> Prog (Either NotWritable (Either EPIPE ()))
  ListPaths :: Prog [Path] -- TODO: rename Paths

  --SavingEnv :: Prog a -> Prog a -- TODO: kill when switch to use Spawn


sim :: Prog () -> Interaction
sim prog = do
  let action = linearize prog (\() -> A_Done)
  let (state,pid) = newPid state0
  resume pid (Proc env0 action) state

linearize :: Prog a -> (a -> Action) -> Action
linearize p0 = case p0 of
  Ret a -> \k -> k a
  Bind p f -> \k ->linearize p $ \a -> linearize (f a) k
  Trace message -> \k -> A_Trace message (k ())
  --Exit -> \_ignoredK -> A_Done
  Spawn child f -> \k -> do
    let action = linearize child $ \() -> A_Done
    A_Spawn action $ \pid -> linearize (f pid) k
  Wait pid -> \k -> A_Wait pid (k ())
  Call sys arg -> \k -> A_Call sys arg k
  Open path mode -> A_Call Sys_Open (path,mode)
  Close fd -> A_Call Sys_Close fd
  Dup2 fd1 fd2 -> A_Call Sys_Dup2 (fd1,fd2)
  Read fd -> A_Call Sys_Read fd
  Write fd line -> A_Call Sys_Write (fd,line)
  ListPaths -> A_Call Sys_Paths ()

  {-SavingEnv prog -> \k -> do
    A_SaveEnv $ \env -> do
      linearize prog $ \a ->
        A_RestoreEnv env (k a)-}

data Action where
  A_Done :: Action
  A_Trace :: String -> Action -> Action
  A_Spawn :: Action -> (Pid -> Action) -> Action
  A_Wait :: Pid -> Action -> Action
  A_Call :: SysCall a b -> a -> (b -> Action) -> Action

  -- TEMP until we move to Spawn/Wait
  {-A_SaveEnv :: (Env -> Action) -> Action
  A_RestoreEnv :: Env -> Action -> Action-}

----------------------------------------------------------------------

resume :: Pid -> Proc -> State -> Interaction
resume me proc0@(Proc env action0) state@State{os} = case action0 of

  A_Done -> do
    case choose state of
      Nothing -> Halt
      Just (state,other,proc2) ->
        resume other proc2 state

  A_Trace message action ->
    TraceLine message (resume me (Proc env action) state)

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
    case runSysI sys os env arg of
      Left Block -> block me proc0 state
      Right proceed -> do
        proceed $ \os env res -> do
          let action = f res
          yield me (Proc env action) state { os }

  --A_SaveEnv f -> resume me (Proc env (f env)) state
  --A_RestoreEnv env action -> resume me (Proc env action) state


block :: Pid -> Proc -> State -> Interaction
block = yield

yield :: Pid -> Proc -> State -> Interaction
yield me proc1 state = do
  case choose state of
    Nothing ->
      --TraceLine (show ("yield",me)) $ do
      resume me proc1 state -- nothing else to do, so continue
    Just (state,other,proc2) ->
      --TraceLine (show ("yield",me,other)) $ do
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

state0 :: State
state0 = State
  { os = os0
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

----------------------------------------------------------------------

type OsState = OsState.State

os0 :: OsState
os0 = OsState.init fs0

-- TODO: move to sep file
fs0 :: FileSystem
fs0 = FileSystem.create
  [ (Path.create "words", File.create ["one","two","three"])
  , (Path.create "test", File.create ["rev < words >> rw", "cat rw"])
  , (Path.create "t", File.create ["echo foo >> xx", "echo bar"])
  ]

----------------------------------------------------------------------
-- TODO: sep file

data SysCall a b where
  Sys_Open :: SysCall (Path,OpenMode) (Either NoSuchPath FD)
  Sys_Close :: SysCall FD ()
  Sys_Dup2 :: SysCall (FD,FD) ()
  Sys_Read :: SysCall FD (Either NotReadable (Either EOF String))
  Sys_Write :: SysCall (FD,String) (Either NotWritable (Either EPIPE ()))
  Sys_Paths :: SysCall () [Path]

runSysI :: SysCall a b ->
  OsState -> Env -> a ->
  Either Block ((OsState -> Env -> b -> Interaction) -> Interaction)

runSysI sys s env arg = case sys of

  Sys_Open -> do
    let (path,mode) = arg
    case OsState.open s path mode of
      Left NoSuchPath -> do
        Right $ \k ->
          k s env (Left NoSuchPath)
      Right (key,s) -> do
        Right $ \k -> do
          let fd = smallestUnused env
          --TraceLine (show ("Open",path,mode,fd)) $ do
          let env' = Map.insert fd (File key) env
          k s env' (Right fd)

  Sys_Close -> do
    let fd = arg
    Right $ \k -> do
      --TraceLine (show ("Close",fd)) $ do
      let env' = Map.delete fd env
      let s' = case look "sim,Close" fd env of
            File key -> OsState.close s key
            Console -> s
      k s' env' ()

  Sys_Dup2 -> do
    let (fdDest,fdSrc) = arg
    Right $ \k -> do
      --TraceLine (show ("Dup2",fdDest,fdSrc)) $ do
      let s' = case look "sim,Dup2,dest" fdDest env of
            File key -> OsState.close s key
            Console -> s
      let target = look "sim,Dup2,src" fdSrc env
      let s'' = case target of
            File key -> OsState.dup s' key
            Console -> s'
      let env' = Map.insert fdDest target env
      k s'' env' ()

  Sys_Read -> do
    let fd = arg
    case look "sim,Read" fd env of
      File key -> do
        case OsState.read s key of
          Left NotReadable -> do
            Right $ \k ->
              k s env (Left NotReadable)
          Right (Left Block) ->
            undefined -- TODO: blocking; when we have pipes
          Right (Right (dat,s)) -> do
            Right $ \k ->
              k s env (Right dat)
      Console -> do
        Right $ \k -> do
          ReadLine $ \case -- TODO: share alts
            Left EOF ->
              k s env (Right (Left EOF))
            Right line ->
              k s env (Right (Right line))

  Sys_Write -> do
    let (fd,line) = arg
    case look "sim,Write" fd env of
      File key -> do
        case OsState.write s key line of
          Left NotWritable -> do
            Right $ \k ->
              k s env (Left NotWritable)
          Right (Left Block) -> do
            undefined -- TODO: blocking; when we have pipes
            --Left Block -- but easy to implement!!
          Right (Right (Left EPIPE)) -> do
            Right $ \k ->
              k s env (Right (Left EPIPE))
          Right (Right (Right s)) -> do
            Right $ \k ->
              k s env (Right (Right ()))
      Console -> do
        Right $ \k ->
          WriteLine line (k s env (Right (Right ())))

  Sys_Paths{} -> do
    Right $ \k -> do
      let paths = OsState.ls s
      k s env paths

----------------------------------------------------------------------

type Env = Map FD Target -- per process state, currently just FD map

data Target
  = Console
  | File OsState.Key
  deriving Show

env0 :: Env
env0 = Map.fromList [ (FD n, Console) | n <- [0,1,2] ]

newtype FD = FD Int
  deriving (Eq,Ord,Enum,Show)

smallestUnused :: Env -> FD
smallestUnused env = head [ fd | fd <- [FD 0..], fd `notElem` used ]
  where used = Map.keys env

-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show ("look/error",tag,k))) id (Map.lookup k env)

----------------------------------------------------------------------
-- TODO: sep file

data Interaction where
  ReadLine :: (Either EOF String -> Interaction) -> Interaction
  WriteLine :: String -> Interaction -> Interaction
  TraceLine :: String -> Interaction -> Interaction
  Halt :: Interaction

