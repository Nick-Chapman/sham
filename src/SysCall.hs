
module SysCall (
  SysCall(..),runSys,
  Env,env0,
  FD(..)
  ) where

import Data.Map (Map)
import FileSystem (NoSuchPath(..))
import Misc (Block(..),EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import OsState (OsState,OpenMode(..))
import Path (Path)
import Interaction (Interaction(..))
import qualified Data.Map.Strict as Map
import qualified OsState (ls,open,Key,close,dup,read,write)

data SysCall a b where
  Open :: SysCall (Path,OpenMode) (Either NoSuchPath FD)
  Close :: SysCall FD ()
  Dup2 :: SysCall (FD,FD) ()
  Read :: SysCall FD (Either NotReadable (Either EOF String))
  Write :: SysCall (FD,String) (Either NotWritable (Either EPIPE ()))
  Paths :: SysCall () [Path]

runSys :: SysCall a b ->
  OsState -> Env -> a ->
  Either Block ((OsState -> Env -> b -> Interaction) -> Interaction)

runSys sys s env arg = case sys of

  Open -> do
    let (path,mode) = arg
    case OsState.open s path mode of
      Left NoSuchPath -> do
        Right $ \k ->
          k s env (Left NoSuchPath)
      Right (key,s) -> do
        Right $ \k -> do
          let fd = smallestUnused env
          --I_Trace (show ("Open",path,mode,fd)) $ do
          let env' = Map.insert fd (File key) env
          k s env' (Right fd)

  Close -> do
    let fd = arg
    Right $ \k -> do
      --I_Trace (show ("Close",fd)) $ do
      let env' = Map.delete fd env
      let s' = case look "sim,Close" fd env of
            File key -> OsState.close s key
            Console -> s
      k s' env' ()

  Dup2 -> do
    let (fdDest,fdSrc) = arg
    Right $ \k -> do
      --I_Trace (show ("Dup2",fdDest,fdSrc)) $ do
      let s' = case look "sim,Dup2,dest" fdDest env of
            File key -> OsState.close s key
            Console -> s
      let target = look "sim,Dup2,src" fdSrc env
      let s'' = case target of
            File key -> OsState.dup s' key
            Console -> s'
      let env' = Map.insert fdDest target env
      k s'' env' ()

  Read -> do
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
          I_Read $ \case -- TODO: share alts
            Left EOF ->
              k s env (Right (Left EOF))
            Right line ->
              k s env (Right (Right line))

  Write -> do
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
          I_Write line (k s env (Right (Right ())))

  Paths{} -> do
    Right $ \k -> do
      let paths = OsState.ls s
      k s env paths

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
