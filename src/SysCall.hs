-- | Interpret a 'system-call' of a MeNicks program, with respect to the process-env & open-files.
module SysCall (
  SysCall,runSys,
  FdEnv,env0,dupEnv,closeEnv,openFiles,
  ) where

import Data.List (intercalate)
import Data.Map (Map)
import Interaction (Interaction(..),OutMode(..))
import OpenFiles (OpenFiles,whatIsKey)
import Prog
import qualified Data.Map.Strict as Map
import qualified OpenFiles

runSys :: SysCall a b ->
  OpenFiles -> FdEnv -> a ->
  Either Block -- TODO: kill unused outer block
  ((OpenFiles -> FdEnv -> (Either Block b) -> Interaction) -> Interaction)

runSys sys s env arg = case sys of

  Kind -> do
    let path = arg
    let res =  OpenFiles.fileKind s path
    Right $ \k -> k s env (Right res)

  LoadBinary -> do
    let path = arg
    let res =  OpenFiles.loadBinary s path
    Right $ \k -> k s env (Right res)

  Fds -> do
    let fd = allFds env
    Right $ \k -> k s env (Right fd)

  SysPipe -> do
    case OpenFiles.pipe s of
      (PipeEnds{r=keyR,w=keyW},s) -> do
        let fdR = smallestUnused env
        env <- pure $ FdEnv (Map.insert fdR (File keyR) (unFdEnv env))
        let fdW = smallestUnused env
        env <- pure $ FdEnv (Map.insert fdW (File keyW) (unFdEnv env))
        let pe = PipeEnds { r = fdR, w = fdW }
        Right $ \k -> k s env (Right pe)

  Open -> do
    let (path,mode) = arg
    case OpenFiles.open s path mode of
      Left err -> do
        Right $ \k ->
          k s env (Right (Left err))
      Right (key,s) -> do
        Right $ \k -> do
          let fd = smallestUnused env
          let env' = FdEnv (Map.insert fd (File key) (unFdEnv env))
          k s env' (Right (Right fd))

  Close -> do
    let fd = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> Right $ \k -> k s env (Right (Left BadFileDescriptor))
      Just thing -> do
        Right $ \k -> do
          case closeTarget thing s
            of s -> do
                 k s (FdEnv (Map.delete fd (unFdEnv env))) (Right (Right ()))

  Dup2 -> do
    let (fdDest,fdSrc) = arg
    Right $ \k -> do
      case Map.lookup fdSrc (unFdEnv env) of
        Nothing -> k s env (Right (Left BadFileDescriptor))
        Just src -> do
          if fdDest == fdSrc then k s env (Right (Right ())) else do
            let
              s' =
                case Map.lookup fdDest (unFdEnv env) of
                  Nothing -> s
                  Just oldTarget -> closeTarget oldTarget s
            let s'' = case src of
                  File key -> OpenFiles.dup s' key
                  Console{}-> s'
            let env' = FdEnv (Map.insert fdDest src (unFdEnv env))
            k s'' env' (Right (Right ()))

  Read prompt -> do
    let fd = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> Right $ \k -> k s env (Right (Left ER_BadFileDescriptor))
      Just thing -> do
        case thing of
          File key -> do
            Right $ \k -> do
              OpenFiles.read prompt s key $ \case
                Left NotReadable ->
                  k s env (Right (Left ER_NotReadable))
                Right (Left Block) ->
                  k s env (Left Block)
                Right (Right (dat,s)) ->
                  k s env (Right (Right dat))

          Console{} -> undefined $ do -- TODO: deprecate this
            Right $ \k -> do
              I_Read prompt $ \case
                Nothing ->
                  k s env (Left Block)
                Just lineOrEOF ->
                  k s env (Right (Right lineOrEOF))


  Write -> do
    let (fd,line) = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> Right $ \k -> k s env (Right (Left EW_BadFileDescriptor))
      Just thing -> do
        case thing of
          File key -> do
            Right $ \k ->
              OpenFiles.write s key line $ \case
                Left NotWritable ->
                  k s env (Right (Left EW_NotWritable))
                Right (Left Block) ->
                  k s env (Left Block)
                Right (Right (Left EPIPE)) ->
                  k s env (Right (Left EW_PIPE))
                Right (Right (Right s)) ->
                  k s env (Right (Right ()))

          Console outMode -> undefined $ do -- TODO: now depreated; kill
            Right $ \k ->
              I_Write outMode line (k s env (Right (Right ())))

  Paths -> do
    Right $ \k -> do
      let paths = OpenFiles.ls s
      k s env (Right paths)

  Mv -> do
    let (src,dest) = arg
    Right $ \k -> do
      case OpenFiles.mv s src dest of
        Left NoSuchPath -> k s env (Right (Left NoSuchPath))
        Right s -> k s env (Right (Right ()))

  Rm -> do
    let path = arg
    Right $ \k -> do
      case OpenFiles.rm s path of
        Left NoSuchPath -> k s env (Right (Left NoSuchPath))
        Right s -> k s env (Right (Right ()))


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
  [
   (FD 2, File OpenFiles.terminalx),
   (FD 1, File OpenFiles.terminal),
   (FD 0, File OpenFiles.terminal)
--   (FD 2, Console StdErr),
--   (FD 1, Console StdOut),
--   (FD 0, Console StdOut)
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
