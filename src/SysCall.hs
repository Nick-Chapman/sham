-- | Interpret a 'system-call' of a MeNicks program, with respect to the process-env & open-files.
module SysCall (
  SysCall,runSys,
  FdEnv,env0,dupEnv,closeEnv,openFiles,
  ) where

import Data.List (intercalate)
import Data.Map (Map)
import Interaction (Interaction(..),OutMode(..))
import Misc (Block(..),EPIPE(..),NotReadable(..),NotWritable(..),PipeEnds(..))
import OpenFiles (OpenFiles,whatIsKey)
import Prog
import qualified Data.Map.Strict as Map
import qualified OpenFiles

runSys :: SysCall a b ->
  OpenFiles -> FdEnv -> a ->
  Either Block ((OpenFiles -> FdEnv -> b -> Interaction) -> Interaction)

runSys sys s env arg = case sys of

  Kind -> do
    let path = arg
    let res =  OpenFiles.fileKind s path
    Right $ \k -> k s env res

  LoadBinary -> do
    let path = arg
    let res =  OpenFiles.loadBinary s path
    Right $ \k -> k s env res

  Unused -> do
    let fd = smallestUnused env
    env <- pure $ FdEnv (Map.insert fd (File OpenFiles.devnull) (unFdEnv env))
    let env' = env
    Right $ \k -> k s env' fd

  Fds -> do
    let fd = allFds env
    Right $ \k -> k s env fd

  SysPipe -> do
    case OpenFiles.pipe s of
      (PipeEnds{r=keyR,w=keyW},s) -> do
        let fdR = smallestUnused env
        env <- pure $ FdEnv (Map.insert fdR (File keyR) (unFdEnv env))
        let fdW = smallestUnused env
        env <- pure $ FdEnv (Map.insert fdW (File keyW) (unFdEnv env))
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
          let env' = FdEnv (Map.insert fd (File key) (unFdEnv env))
          k s env' (Right fd)

  Close -> do
    let fd = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> Right $ \k -> k s env (Left BadFileDescriptor)
      Just thing -> do
        Right $ \k -> do
          case closeTarget thing s
            of s -> do
                 k s (FdEnv (Map.delete fd (unFdEnv env))) (Right ())

  Dup2 -> do
    let (fdDest,fdSrc) = arg
    Right $ \k -> do
      case Map.lookup fdSrc (unFdEnv env) of
        Nothing -> k s env (Left BadFileDescriptor)
        Just src -> do
          if fdDest == fdSrc then k s env (Right ()) else do
            let
              s' =
                case Map.lookup fdDest (unFdEnv env) of
                  Nothing -> s
                  Just oldTarget -> closeTarget oldTarget s
            let s'' = case src of
                  File key -> OpenFiles.dup s' key
                  Console{}-> s'
            let env' = FdEnv (Map.insert fdDest src (unFdEnv env))
            k s'' env' (Right ())

  Read prompt -> do
    let fd = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> Right $ \k -> k s env (Left ER_BadFileDescriptor)
      Just thing -> do
        case thing of
          File key -> do
            case OpenFiles.read s key of
              Left NotReadable -> do
                Right $ \k ->
                  k s env (Left ER_NotReadable)
              Right (Left Block) ->
                Left Block
              Right (Right (dat,s)) -> do
                Right $ \k ->
                  k s env (Right dat)
          Console{} -> do
            Right $ \k -> do
              I_Read prompt $ \lineOrEOF -> do
                k s env (Right lineOrEOF)

  Write -> do
    let (fd,line) = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> Right $ \k -> k s env (Left EW_BadFileDescriptor)
      Just thing -> do
        case thing of
          File key -> do
            case OpenFiles.write s key line of
              Left NotWritable -> do
                Right $ \k ->
                  k s env (Left EW_NotWritable)
              Right (Left Block) -> do
                Left Block
              Right (Right (Left EPIPE)) -> do
                Right $ \k ->
                  k s env (Left EW_PIPE)
              Right (Right (Right s)) -> do
                Right $ \k ->
                  k s env (Right ())

          Console outMode -> undefined $ do -- TODO: kill
            Right $ \k ->
              I_Write outMode line (k s env (Right ()))

  Paths{} -> do
    Right $ \k -> do
      let paths = OpenFiles.ls s
      k s env paths

  Mv{} -> do
    let (src,dest) = arg
    Right $ \k -> do
      case OpenFiles.mv s src dest of
        Left NoSuchPath -> k s env (Left NoSuchPath)
        Right s -> k s env (Right ())

-- TODO: split out FdEnv into new module
newtype FdEnv = FdEnv { unFdEnv :: Map FD Target } -- per process state, currently just FD map

data Target
  = Console OutMode -- TODO: plan to deprecate this, and handle console via tty/pipes
  | File OpenFiles.Key

instance Show FdEnv where
  show FdEnv{unFdEnv=m} =
    intercalate ", " [ show k ++ "=" ++ show e | (k,e) <- Map.toList m ]

instance Show Target where
  show = \case
    Console StdOut -> "Tx"
    Console StdErr -> "Te"
    File k -> show k

openFiles :: OpenFiles -> FdEnv -> [(FD,OF)]
openFiles os (FdEnv m) =
  [ (fd,oF) | (fd,File key) <- Map.toList m, let oF = OpenFiles.whatIsKey os key ]

env0 :: FdEnv
env0 = FdEnv $ Map.fromList
  [ (FD n, Console m)
  | (n,m) <-
    [
      (0,StdOut)
--    , (1,StdOut)
--    , (2,StdErr)
    ]
  ]

dupEnv :: FdEnv -> OpenFiles -> OpenFiles
dupEnv (FdEnv m) s =
  foldr dupTarget s [ t | (_,t) <- Map.toList m ]

closeEnv :: FdEnv -> OpenFiles -> OpenFiles
closeEnv (FdEnv m) s =
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

smallestUnused :: FdEnv -> FD
smallestUnused (FdEnv m) = head [ fd | fd <- [FD 0..], fd `notElem` used ]
  where used = Map.keys m

allFds :: FdEnv -> [FD]
allFds (FdEnv m) = Map.keys m
