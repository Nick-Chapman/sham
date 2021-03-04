module Os (
  FD(..),Prog(..),
  OpenMode(..),NoSuchPath(..),
  sim,
  Interaction(..)
  ) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import FileSystem (FileSystem,NoSuchPath(..))
import Misc (Block(..),EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import OsState (State,OpenMode(..))
import Path (Path)
import qualified Data.Map.Strict as Map
import qualified File (create)
import qualified FileSystem (create)
import qualified OsState (init,ls,open,Key,close,dup,read,write)
import qualified Path (create)

data Prog a where
  Ret :: a -> Prog a
  Bind :: Prog a -> (a -> Prog b) -> Prog b
  Open :: Path -> OpenMode -> Prog (Either NoSuchPath FD)
  Close :: FD -> Prog ()
  Read :: FD -> Prog (Either NotReadable (Either EOF String))
  Write :: FD -> String -> Prog (Either NotWritable (Either EPIPE ()))
  Trace :: String -> Prog ()
  Ls :: Prog [Path]
  Dup2 :: FD -> FD -> Prog ()
  SavingEnv :: Prog a -> Prog a -- TEMP until we get Fork/Exec

{- TODO
- dup FDs
- Fork processes (duping open FDs) & Exec
- Exit/Wait
-}


instance Functor Prog where fmap = liftM
instance Applicative Prog where pure = return; (<*>) = ap
instance Monad Prog where return = Ret; (>>=) = Bind

sim :: Prog () -> Interaction
sim p0 = loop env0 state0 p0 k0 where

  k0 :: Env -> State -> a -> Interaction
  k0 _ _ _ = Halt

  env0 :: Env
  env0 = Map.fromList [ (FD n, Console) | n <- [0,1,2] ]

  loop :: Env -> State -> Prog a -> (Env -> State -> a -> Interaction) -> Interaction
  loop env s prog0 k = case prog0 of
    Ret a -> k env s a
    Bind prog f -> loop env s prog $ \env s a -> loop env s (f a) k

    Open path mode -> do
      --TraceLine (show env) $ do
      case OsState.open s path mode of
        Left NoSuchPath -> k env s (Left NoSuchPath)
        Right (key,s) -> do
          let fd = smallestUnused env
          TraceLine (show ("Open",path,mode,fd)) $ do
          let env' = Map.insert fd (File key) env
          --TraceLine (show env') $ do
          k env' s (Right fd)

    Close fd -> do
      --TraceLine (show env) $ do
      TraceLine (show ("Close",fd)) $ do
      let env' = Map.delete fd env
      let s' = case look "sim,Close" fd env of
            File key -> OsState.close s key
            Console -> s
      --TraceLine (show env') $ do
      k env' s' ()

    Dup2 fdDest fdSrc -> do
      TraceLine (show ("Dup2",fdDest,fdSrc)) $ do
      let s' = case look "sim,Dup2,dest" fdDest env of
            File key -> OsState.close s key
            Console -> s
      let target = look "sim,Dup2,src" fdSrc env
      let s'' = case target of
            File key -> OsState.dup s' key
            Console -> s'
      let env' = Map.insert fdDest target env
      k env' s'' ()

    SavingEnv prog ->
      loop env s prog $ \_ s a -> k env s a

    Read fd -> do
      case look "sim,Read" fd env of
        File key -> do
          case OsState.read s key of
            Left NotReadable -> k env s (Left NotReadable)
            Right (Left Block) -> undefined -- TODO: blocking; when we have pipes
            Right (Right (dat,s')) -> k env s' (Right dat)
        Console -> do
          ReadLine $ \case
            Left EOF -> k env s (Right (Left EOF))
            Right line -> k env s (Right (Right line))

    Write fd line -> do
      case look "sim,Write" fd env of
        File key -> do
          case OsState.write s key line of
            Left NotWritable -> k env s (Left NotWritable)
            Right (Left Block) -> undefined -- TODO: blocking; when we have pipes
            Right (Right (Left EPIPE)) -> k env s (Right (Left EPIPE))
            Right (Right (Right s')) -> k env s' (Right (Right ()))
        Console -> do
          WriteLine line (k env s (Right (Right ())))

    Trace message -> do
      TraceLine message (k env s ())

    Ls -> do
      let paths = OsState.ls s
      k env s paths


newtype FD = FD Int
  deriving (Eq,Ord,Enum,Show)

state0 :: State
state0 = OsState.init fs0

fs0 :: FileSystem
fs0 = FileSystem.create
  [ (Path.create "words", File.create ["one","two","three"])
  , (Path.create "test1", File.create ["echo test1...","ls","cat words","cat xxx"])
  , (Path.create "test2", File.create ["echo something >> newFile"])

  , (Path.create "e3", File.create ["echo first","echo second"])
  , (Path.create "test", File.create ["e3 >> newFile"])
  ]

type Env = Map FD Target

smallestUnused :: Env -> FD
smallestUnused env = head [ fd | fd <- [FD 0..], fd `notElem` used ]
  where used = Map.keys env

data Target
  = Console
  | File OsState.Key
  deriving Show

data Interaction where
  ReadLine :: (Either EOF String -> Interaction) -> Interaction
  WriteLine :: String -> Interaction -> Interaction
  TraceLine :: String -> Interaction -> Interaction
  Halt :: Interaction


-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show ("look/error",tag,k))) id (Map.lookup k env)
