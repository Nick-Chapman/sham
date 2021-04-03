-- | Interpret a 'system-call' of a MeNicks program, with respect to the process-env & open-files.
module SysCall (SysCall,runSys,makeEnv,dupEnv,closeEnv,openFiles) where

import Interaction (Interaction(..))
import Kernel (State(..),FdEnv(..))
import OpenFileTable (OpenFileTable,whatIsKey)
import Prog
import qualified Data.Map.Strict as Map
import qualified OpenFileTable

runSys :: SysCall a b ->
  State -> FdEnv -> a ->
  ((State -> FdEnv -> (Either Block b) -> Interaction) -> Interaction)

runSys sys s@State{oft} env arg = case sys of

  Kind -> do
    let path = arg
    let res =  OpenFileTable.fileKind oft path
    \k -> k s env (Right res)

  LoadBinary -> do
    let path = arg
    let res =  OpenFileTable.loadBinary oft path
    \k -> k s env (Right res)

  Fds -> do
    let fd = allFds env
    \k -> k s env (Right fd)

  SysPipe -> do
    case OpenFileTable.pipe oft of
      (PipeEnds{r=keyR,w=keyW},oft) -> do
        let fdR = smallestUnused env
        env <- pure $ FdEnv (Map.insert fdR keyR (unFdEnv env))
        let fdW = smallestUnused env
        env <- pure $ FdEnv (Map.insert fdW keyW (unFdEnv env))
        let pe = PipeEnds { r = fdR, w = fdW }
        \k -> k s { oft } env (Right pe)

  Open -> do
    let (path,mode) = arg
    case OpenFileTable.open oft path mode of
      Left err -> do
        \k ->
          k s env (Right (Left err))
      Right (key,oft) -> do
        \k -> do
          let fd = smallestUnused env
          let env' = FdEnv (Map.insert fd key (unFdEnv env))
          k s { oft } env' (Right (Right fd))

  Close -> do
    let fd = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> \k -> k s env (Right (Left BadFileDescriptor))
      Just key -> do
        \k -> do
          case OpenFileTable.close oft key of
            oft -> do
              k s { oft } (FdEnv (Map.delete fd (unFdEnv env))) (Right (Right ()))

  Dup2 -> do
    let (fdDest,fdSrc) = arg
    \k -> do
      case Map.lookup fdSrc (unFdEnv env) of
        Nothing -> k s env (Right (Left BadFileDescriptor))
        Just srcKey -> do
          if fdDest == fdSrc
            then k s env (Right (Right ())) else do
            let
              oft1 =
                case Map.lookup fdDest (unFdEnv env) of
                  Nothing -> oft
                  Just oldKey -> OpenFileTable.close oft oldKey
            let oft2 = OpenFileTable.dup oft1 srcKey
            let env' = FdEnv (Map.insert fdDest srcKey (unFdEnv env))
            k s { oft = oft2 } env' (Right (Right ()))

  Read prompt -> do
    let fd = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> \k -> k s env (Right (Left ER_BadFileDescriptor))
      Just key -> do
        \k -> do
          OpenFileTable.read prompt oft key $ \case
            Left NotReadable ->
              k s env (Right (Left ER_NotReadable))
            Right (Left Block) ->
              k s env (Left Block)
            Right (Right (dat,oft)) ->
              k s { oft } env (Right (Right dat))

  Write -> do
    let (fd,line) = arg
    case Map.lookup fd (unFdEnv env) of
      Nothing -> \k -> k s env (Right (Left EW_BadFileDescriptor))
      Just key -> do
        \k ->
          OpenFileTable.write oft key line $ \case
            Left NotWritable ->
              k s env (Right (Left EW_NotWritable))
            Right (Left Block) ->
              k s env (Left Block)
            Right (Right (Left EPIPE)) ->
              k s env (Right (Left EW_PIPE))
            Right (Right (Right oft)) ->
              k s { oft } env (Right (Right ()))

  Paths -> do
    \k -> do
      let paths = OpenFileTable.ls oft
      k s env (Right paths)

  Mv -> do
    let (src,dest) = arg
    \k -> do
      case OpenFileTable.mv oft src dest of
        Left NoSuchPath -> k s env (Right (Left NoSuchPath))
        Right oft -> k s { oft } env (Right (Right ()))

  Rm -> do
    let path = arg
    \k -> do
      case OpenFileTable.rm oft path of
        Left NoSuchPath -> k s env (Right (Left NoSuchPath))
        Right oft -> k s { oft } env (Right (Right ()))


openFiles :: OpenFileTable -> FdEnv -> [(FD,OF)]
openFiles oft (FdEnv m) =
  [ (fd,oF) | (fd,key) <- Map.toList m, let oF = OpenFileTable.whatIsKey oft key ]

makeEnv :: [ (FD,OpenFileTable.Key) ] -> FdEnv
makeEnv xs = FdEnv $ Map.fromList xs

dupEnv :: FdEnv -> OpenFileTable -> OpenFileTable
dupEnv (FdEnv m) s =
  foldl OpenFileTable.dup s [ t | (_,t) <- Map.toList m ]

closeEnv :: FdEnv -> OpenFileTable -> OpenFileTable
closeEnv (FdEnv m) s =
  foldl OpenFileTable.close s [ t | (_,t) <- Map.toList m ]

smallestUnused :: FdEnv -> FD
smallestUnused (FdEnv m) = head [ fd | fd <- [FD 0..], fd `notElem` used ]
  where used = Map.keys m

allFds :: FdEnv -> [FD]
allFds (FdEnv m) = Map.keys m
