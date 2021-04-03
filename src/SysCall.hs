-- | Interpret a 'system-call' of a MeNicks program, with respect to the process-env & open-files.
module SysCall (SysCall,runSys,makeEnv,dupEnv,closeEnv,openFiles) where

import Interaction (Interaction(..))
import Kernel (FdEnv(..))
import OpenFiles (OpenFiles,whatIsKey)
import Prog
import qualified Data.Map.Strict as Map
import qualified OpenFiles

runSys :: SysCall a b ->
  OpenFiles -> FdEnv -> a ->
  ((OpenFiles -> FdEnv -> (Either Block b) -> Interaction) -> Interaction)

runSys sys s env arg = case sys of

  Kind -> do
    let path = arg
    let res =  OpenFiles.fileKind s path
    \k -> k s env (Right res)

  LoadBinary -> do
    let path = arg
    let res =  OpenFiles.loadBinary s path
    \k -> k s env (Right res)

  Fds -> do
    let fd = allFds env
    \k -> k s env (Right fd)

  SysPipe -> do
    case OpenFiles.pipe s of
      (PipeEnds{r=keyR,w=keyW},s) -> do
        let fdR = smallestUnused env
        env <- pure $ FdEnv (Map.insert fdR keyR (unFdEnv env))
        let fdW = smallestUnused env
        env <- pure $ FdEnv (Map.insert fdW keyW (unFdEnv env))
        let pe = PipeEnds { r = fdR, w = fdW }
        \k -> k s env (Right pe)

  Open -> do
    let (path,mode) = arg
    case OpenFiles.open s path mode of
      Left err -> do
        \k ->
          k s env (Right (Left err))
      Right (key,s) -> do
        \k -> do
          let fd = smallestUnused env
          let env' = FdEnv (Map.insert fd key (unFdEnv env))
          k s env' (Right (Right fd))

  Close -> do
    let fd = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> \k -> k s env (Right (Left BadFileDescriptor))
      Just key -> do
        \k -> do
          case OpenFiles.close s key
            of s -> do
                 k s (FdEnv (Map.delete fd (unFdEnv env))) (Right (Right ()))

  Dup2 -> do
    let (fdDest,fdSrc) = arg
    \k -> do
      case Map.lookup fdSrc (unFdEnv env) of
        Nothing -> k s env (Right (Left BadFileDescriptor))
        Just srcKey -> do
          if fdDest == fdSrc
            then k s env (Right (Right ())) else do
            let
              s' =
                case Map.lookup fdDest (unFdEnv env) of
                  Nothing -> s
                  Just oldKey -> OpenFiles.close s oldKey
            let s'' = OpenFiles.dup s' srcKey
            let env' = FdEnv (Map.insert fdDest srcKey (unFdEnv env))
            k s'' env' (Right (Right ()))

  Read prompt -> do
    let fd = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> \k -> k s env (Right (Left ER_BadFileDescriptor))
      Just key -> do
        \k -> do
          OpenFiles.read prompt s key $ \case
            Left NotReadable ->
              k s env (Right (Left ER_NotReadable))
            Right (Left Block) ->
              k s env (Left Block)
            Right (Right (dat,s)) ->
              k s env (Right (Right dat))

  Write -> do
    let (fd,line) = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> \k -> k s env (Right (Left EW_BadFileDescriptor))
      Just key -> do
        \k ->
          OpenFiles.write s key line $ \case
            Left NotWritable ->
              k s env (Right (Left EW_NotWritable))
            Right (Left Block) ->
              k s env (Left Block)
            Right (Right (Left EPIPE)) ->
              k s env (Right (Left EW_PIPE))
            Right (Right (Right s)) ->
              k s env (Right (Right ()))

  Paths -> do
    \k -> do
      let paths = OpenFiles.ls s
      k s env (Right paths)

  Mv -> do
    let (src,dest) = arg
    \k -> do
      case OpenFiles.mv s src dest of
        Left NoSuchPath -> k s env (Right (Left NoSuchPath))
        Right s -> k s env (Right (Right ()))

  Rm -> do
    let path = arg
    \k -> do
      case OpenFiles.rm s path of
        Left NoSuchPath -> k s env (Right (Left NoSuchPath))
        Right s -> k s env (Right (Right ()))


openFiles :: OpenFiles -> FdEnv -> [(FD,OF)]
openFiles os (FdEnv m) =
  [ (fd,oF) | (fd,key) <- Map.toList m, let oF = OpenFiles.whatIsKey os key ]

makeEnv :: [ (FD,OpenFiles.Key) ] -> FdEnv
makeEnv xs = FdEnv $ Map.fromList xs

dupEnv :: FdEnv -> OpenFiles -> OpenFiles
dupEnv (FdEnv m) s =
  foldl OpenFiles.dup s [ t | (_,t) <- Map.toList m ]

closeEnv :: FdEnv -> OpenFiles -> OpenFiles
closeEnv (FdEnv m) s =
  foldl OpenFiles.close s [ t | (_,t) <- Map.toList m ]

smallestUnused :: FdEnv -> FD
smallestUnused (FdEnv m) = head [ fd | fd <- [FD 0..], fd `notElem` used ]
  where used = Map.keys m

allFds :: FdEnv -> [FD]
allFds (FdEnv m) = Map.keys m
