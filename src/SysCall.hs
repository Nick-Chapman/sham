module SysCall (
  SysCall(..),runSys,
  Env,env0,dupEnv,closeEnv,
  FD(..), BadFileDescriptor(..), PipeEnds(..),OpenError(..), LoadBinaryError(..),
  ) where

import Data.List (intercalate)
import Data.Map (Map)
import Interaction (Interaction(..),Prompt,OutMode(..))
import Misc (Block(..),EOF(..),EPIPE(..),NotReadable(..),NotWritable(..),PipeEnds(..))
import OpenFiles (OpenFiles,OpenMode(..),OpenError(..),LoadBinaryError(..))
import Path (Path)
import qualified Data.Map.Strict as Map
import qualified OpenFiles (ls,open,pipe,Key,close,dup,read,write,devnull,loadBinary)
import qualified File (Prog)

data SysCall a b where
  LoadBinary :: SysCall Path (Either LoadBinaryError File.Prog)
  Open :: SysCall (Path,OpenMode) (Either OpenError FD)
  Close :: SysCall FD ()
  Dup2 :: SysCall (FD,FD) (Either BadFileDescriptor ())
  Read :: Prompt -> SysCall FD (Either NotReadable (Either EOF String))
  Write :: SysCall (FD,String) (Either NotWritable (Either EPIPE ()))
  Paths :: SysCall () [Path]
  SysPipe :: SysCall () (PipeEnds FD)
  Unused :: SysCall () FD -- TODO: for used for by with redirectsa

data BadFileDescriptor = BadFileDescriptor deriving Show

instance Show (SysCall a b) where -- TODO: automate?
  show = \case
    LoadBinary -> "LoadBinary"
    Open -> "Open"
    Close -> "Close"
    Dup2 -> "Dup2"
    Read _ -> "Read"
    Write -> "Write"
    Paths -> "Paths"
    SysPipe -> "Pipe"
    Unused -> "Unused"

runSys :: SysCall a b ->
  OpenFiles -> Env -> a ->
  Either Block ((OpenFiles -> Env -> b -> Interaction) -> Interaction)

runSys sys s env arg = case sys of

  LoadBinary -> do
    let path = arg
    let res =  OpenFiles.loadBinary s path
    Right $ \k -> k s env res

  Unused -> do
    let fd = smallestUnused env
    env <- pure $ Env (Map.insert fd (File OpenFiles.devnull) (unEnv env))
    let env' = env
    Right $ \k -> k s env' fd

  SysPipe -> do
    case OpenFiles.pipe s of
      (PipeEnds{r=keyR,w=keyW},s) -> do
        let fdR = smallestUnused env
        env <- pure $ Env (Map.insert fdR (File keyR) (unEnv env))
        let fdW = smallestUnused env
        env <- pure $ Env (Map.insert fdW (File keyW) (unEnv env))
        let pe = PipeEnds { r = fdR, w = fdW }
        Right $ \k -> k s env pe

  Open -> do
    let (path,mode) = arg
    case OpenFiles.open s path mode of
      Left err -> do
        Right $ \k ->
          k s env (Left err)
      Right (key,s) -> do
        Right $ \k -> do
          let fd = smallestUnused env
          let env' = Env (Map.insert fd (File key) (unEnv env))
          k s env' (Right fd)

  Close -> do
    let fd = arg
    Right $ \k -> do
      case (case look "Sys.Close" fd (unEnv env) of
              File key -> OpenFiles.close s key
              Console{} -> (False,s))
        of (_closing,s) -> do
             --(if closing then (I_Trace "**CLOSED**") else id) $ do
             k s (Env (Map.delete fd (unEnv env))) ()

  Dup2 -> do
    let (fdDest,fdSrc) = arg
    Right $ \k -> do
      if fdDest == fdSrc then k s env (Right ()) else do
      let
        (_closing,s') = --TODO: does this always get forced?
          case Map.lookup fdDest (unEnv env) of
            Nothing -> (False,s)
            Just oldTarget ->
              case oldTarget of
                File key -> OpenFiles.close s key
                Console{} -> (False,s)
      --(if closing then (I_Trace "**CLOSED (by dup2)**") else id) $ do
      case Map.lookup fdSrc (unEnv env) of
        Nothing -> k s' env (Left BadFileDescriptor)
        Just target -> do
          let s'' = case target of
                File key -> OpenFiles.dup s' key
                Console{}-> s'
          let env' = Env (Map.insert fdDest target (unEnv env))
          k s'' env' (Right ())

  Read prompt -> do
    let fd = arg
    case look "Sys.Read" fd (unEnv env) of
      File key -> do
        case OpenFiles.read s key of
          Left NotReadable -> do
            Right $ \k ->
              k s env (Left NotReadable)
          Right (Left Block) ->
            Left Block
          Right (Right (dat,s)) -> do
            Right $ \k ->
              k s env (Right dat)
      Console{} -> do
        Right $ \k -> do
          I_Read prompt $ \case -- TODO: share alts
            Left EOF ->
              k s env (Right (Left EOF))
            Right line ->
              k s env (Right (Right line))

  Write -> do
    let (fd,line) = arg
    case look "Sys.Write" fd (unEnv env) of
      File key -> do
        case OpenFiles.write s key line of
          Left NotWritable -> do
            Right $ \k ->
              k s env (Left NotWritable)
          Right (Left Block) -> do
            Left Block
          Right (Right (Left EPIPE)) -> do
            Right $ \k ->
              k s env (Right (Left EPIPE))
          Right (Right (Right s)) -> do
            Right $ \k ->
              k s env (Right (Right ()))
      Console outMode -> do
        Right $ \k ->
          I_Write outMode line (k s env (Right (Right ())))

  Paths{} -> do
    Right $ \k -> do
      let paths = OpenFiles.ls s
      k s env paths

-- TODO: split out Env into new module
newtype Env = Env { unEnv :: Map FD Target } -- per process state, currently just FD map

data Target
  = Console OutMode -- TODO: plan to deprecate this, and handle console via tty/pipes
  | File OpenFiles.Key

instance Show Env where
  show Env{unEnv=m} =
    intercalate ", " [ show k ++ "=" ++ show e | (k,e) <- Map.toList m ]

instance Show Target where
  show = \case
    Console Normal -> "Tx"
    Console StdErr -> "Te"
    File k -> show k

env0 :: Env
env0 = Env $ Map.fromList
  [ (FD n, Console m) | (n,m) <- [(0,Normal),(1,Normal),(2,StdErr)] ]

dupEnv :: Env -> OpenFiles -> OpenFiles
dupEnv (Env m) s =
  foldr dupTarget s [ t | (_,t) <- Map.toList m ]

closeEnv :: Env -> OpenFiles -> OpenFiles
closeEnv (Env m) s =
  foldr closeTarget s [ t | (_,t) <- Map.toList m ]

-- TOOD: use {dup,close}Target in code above, for better sharing
dupTarget :: Target -> OpenFiles -> OpenFiles
dupTarget tar s =
  case tar of
    File key -> OpenFiles.dup s key
    Console{}-> s

closeTarget :: Target -> OpenFiles -> OpenFiles
closeTarget tar s =
  case tar of
    File key -> snd (OpenFiles.close s key)
    Console{}-> s

newtype FD = FD Int
  deriving (Eq,Ord,Enum,Num)

instance Show FD where show (FD n) = "&" ++ show n

smallestUnused :: Env -> FD
smallestUnused (Env m) = head [ fd | fd <- [FD 0..], fd `notElem` used ]
  where used = Map.keys m

-- TODO: dont error if file-descriptor cannot be found. user can cause this
look :: (Show k, Ord k) => String -> k -> Map k b -> b
look tag k env = maybe (error (show ("look/error",tag,k))) id (Map.lookup k env)
