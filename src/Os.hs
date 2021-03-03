module Os (FD(..),Prog(..),sim,Interaction(..)) where

import Misc (EOF(..),EPIPE(..),NotReadable(..),NotWritable(..))
import Control.Monad (ap,liftM)
import Data.Map (Map)
import FileTable (Fs)
import qualified Data.Map.Strict as Map
import qualified FileTable (empty) --,Key)

data Prog a where
  Ret :: a -> Prog a
  Bind :: Prog a -> (a -> Prog b) -> Prog b
  Read :: FD -> Prog (Either NotReadable (Either EOF String))
  Write :: FD -> String -> Prog (Either NotWritable (Either EPIPE ()))
  Trace :: String -> Prog ()

instance Functor Prog where fmap = liftM
instance Applicative Prog where pure = return; (<*>) = ap
instance Monad Prog where return = Ret; (>>=) = Bind

sim :: Prog () -> Interaction
sim p0 = loop state0 p0 (\_ _ -> Halt) where

  env :: Env
  env = Map.fromList [ (FD n, Console) | n <- [0,1,2] ]

  loop :: State -> Prog a -> (State -> a -> Interaction) -> Interaction
  loop s prog0 k = case prog0 of
    Ret a -> k s a
    Bind prog f -> loop s prog $ \s a -> loop s (f a) k

    Read fd -> do
      case look "sim,Read" fd env of
        Console -> do
          ReadLine $ \case
            Left EOF -> k s (Right (Left EOF))
            Right line -> k s (Right (Right line))

    Write fd line -> do
      case look "sim,Write" fd env of
        Console -> do
          WriteLine line (k s (Right (Right ())))

    Trace message -> do
      TraceLine message (k s ())


newtype FD = FD Int
  deriving (Eq,Ord,Show)

data State = State { fs :: Fs }

state0 :: State
state0 = State { fs = FileTable.empty}

type Env = Map FD Target

data Target
  = Console
--  | File FileTable.Key

data Interaction where
  ReadLine :: (Either EOF String -> Interaction) -> Interaction
  WriteLine :: String -> Interaction -> Interaction
  TraceLine :: String -> Interaction -> Interaction
  Halt :: Interaction


-- helper for map lookup
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show (tag,k))) id (Map.lookup k env)
