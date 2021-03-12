
module Sham (Bins(..),sham,runCommand) where

import Control.Monad (when)
import Data.Map (Map)
import EarleyM (Gram,fail,alts,getToken,many,skipWhile)
import Interaction (Prompt(..))
import Misc (EOF(..),PipeEnds(..))
import Native (withOpen,err2)
import Path (Path)
import Prelude hiding (Word,read,fail)
import Prog (Prog,Pid(..),Command(..),SysCall(..),OpenMode(..),WriteOpenMode(..),FD(..))
import SysCall (BadFileDescriptor(..))
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified EarleyM as EM (parse,Parsing(..))
import qualified Native (read,write,readAll,checkNoArgs)
import qualified Path (create)
import qualified Prog (Prog(..))


data Script
  = Null
  | ShamError String
  -- TODO: sequencing operator ";"
  | Pipe Script Script
  | BuiltinExit
  | BuiltinExec Word [Word]
  | BuiltinEcho [Word] -- run's in same process
  | BuiltinSource Path
  | Run Word [Word] [Redirect] WaitMode
  deriving Show

data Word = Word String | DolDol
  deriving (Eq,Show)

data WaitMode = NoWait | Wait
  deriving (Eq,Show)

data Redirect
  = Redirect OpenMode FD RedirectSource
  deriving Show

data RedirectSource
  = FromPath Path
  | FromFD FD
  deriving Show

newtype Bins = Bins (Map String (Prog ()))

lookupBins :: Bins -> String -> Maybe (Prog ())
lookupBins (Bins m) k = Map.lookup k m

sham :: Int -> Bins -> Prog ()
sham level bins = Native.checkNoArgs $ loop 1 where
  loop :: Int -> Prog ()
  loop n = do
    let prompt = "sham[" ++ show level ++ "." ++ show n ++ "]$ "
    Native.read (Prompt prompt) (FD 0) >>= \case
      Left EOF -> pure ()
      Right line -> do
        let script = parseLine line
        --Prog.Trace (show script)
        interpret bins script
        loop (n+1)

interpret :: Bins -> Script -> Prog ()
interpret bins = \case
  Null -> pure ()
  ShamError message -> err2 message
  BuiltinExit -> Prog.Exit
  BuiltinExec com args -> doExec bins com args
  BuiltinEcho args -> builtinEcho args
  BuiltinSource path -> runShamScript bins path
  Run com args rs mode -> executeCommand bins com args rs mode
  Pipe script1 script2 -> pipe (interpret bins script1) (interpret bins script2)


pipe :: Prog () -> Prog () -> Prog ()
pipe prog1 prog2 = do
  PipeEnds{r,w} <- Prog.Call SysPipe ()
  let command = Command ("sham",[])
  spawn1 command (do -- TODO: dont loose the name of the actual pipe element
               dup2 (FD 1) w
               Prog.Call Close w
               Prog.Call Close r
               prog1
           ) $ \child1 -> do
    spawn1 command (do
                 dup2 (FD 0) r
                 Prog.Call Close w
                 Prog.Call Close r
                 prog2
             ) $ \child2 -> do
      Prog.Call Close w
      Prog.Call Close r
      Prog.Wait child1
      Prog.Wait child2

spawn1 :: Command -> Prog () -> (Pid -> Prog a) -> Prog a
spawn1 command child parent = do
  Prog.Fork >>= \case
    Nothing -> Prog.Exec command child
    Just pid -> parent pid


builtinEcho :: [Word] -> Prog ()
builtinEcho args = do
  args <- mapM evalWord args
  Native.write (FD 1) (unwords args)

executeCommand :: Bins -> Word -> [Word] -> [Redirect] -> WaitMode -> Prog ()
executeCommand bins com args rs mode = do
  Prog.Fork >>= \case
    Nothing -> do
      mapM_ execRedirect rs
      doExec bins com args
    Just pid -> case mode of
      Wait -> Prog.Wait pid
      NoWait -> pure ()

doExec :: Bins -> Word -> [Word] -> Prog ()
doExec bins com args = do
  com <- evalWord com
  args <- mapM evalWord args
  runCommand bins $ Command (com,args)

runCommand :: Bins -> Command -> Prog ()
runCommand bins command = do
  let Command (com,_) = command
  let prog =
        case lookupBins bins com of
          Just prog -> prog
          Nothing -> runShamScript bins (Path.create com)
  Prog.Exec command prog

evalWord :: Word -> Prog String
evalWord = \case
  Word s -> pure s
  DolDol -> do Pid n <- Prog.MyPid; pure (show n)

runShamScript :: Bins -> Path -> Prog ()
runShamScript bins path = do
  lines <- do
    withOpen path OpenForReading $ \fd -> do
      Native.readAll fd
  sequence_ [ interpret bins (parseLine line) | line <- lines ]

execRedirect :: Redirect -> Prog ()
execRedirect r =
  case r of
    Redirect mode dest (FromPath path) -> do
      withOpen path mode $ \src -> do
        dup2 dest src
    Redirect _mode dest (FromFD src) -> do -- do we care what the mode is?
      dup2 dest src

dup2 :: FD -> FD -> Prog ()
dup2 d s = do
  Prog.Call Dup2 (d,s) >>= \case
    Left BadFileDescriptor -> do
      err2 $ "bad file descriptor: " ++ show s
      Prog.Exit
    Right () -> pure ()


----------------------------------------------------------------------
-- syntax...

parseLine :: String -> Script
parseLine str = do
  case EM.parse (lang <$> getToken) str of
    EM.Parsing{EM.outcome} -> case outcome of
      Left pe -> ShamError $ show pe -- TODO: improve parse error message for humans
      Right script -> script


lang :: Gram Char -> Gram Script
lang token = script where

  script = do ws; alts [ do res <- alts [ exit, exec, source, pipeline ]; ws; pure res
                       , do eps; pure Null ]

  -- TODO: builtin exit/exec/source -- should be allowed a pipe-stage ? (like builtin-echo)
  exit = do keyword "exit"; pure BuiltinExit
  exec = do keyword "exec"; ws1; (com,args) <- parseListSep word ws1; pure $ BuiltinExec com args
  source = do keyword "."; ws1; p <- path; pure $ BuiltinSource p

  pipeline = do
    (com1,coms) <- parseListSep pipeStage (do ws; symbol '|'; ws)
    pure $ foldl Pipe com1 coms

  pipeStage = alts [echo,command]

  -- builtin echo runs in process. critical for "yes | head"
  echo = do keyword "echo"
            alts [ do ws1; (com,args) <- parseListSep word ws1; pure $ BuiltinEcho (com:args)
                 , do eps; pure $ BuiltinEcho []
                 ]

  command = do
    (com,args) <- parseListSep word ws1
    when (com `elem` map Word ["exit","exec"]) fail -- give way to syntax
    rs <- redirects
    mode <- alts [ do eps; pure Wait,
                   do ws; symbol '&'; pure NoWait ]
    when (com == Word "echo" && null rs && mode == Wait) fail -- give way to builtin echo
    pure $ Run com args rs mode

  redirects = alts
    -- TODO: Goal: allow just "ws" to separate args from redirects.
    -- Problem is that currently this causes ambiguity for examples such as:
    --  "echo foo1>xx"
    -- It should parse as:       "echo foo1 >xx"
    -- But we think it might be  "echo foo 1>xx"  !!
    [ do ws1; (r1,rs) <- parseListSep redirect ws1; pure (r1:rs)
    , do eps; pure []
    ]

  redirect = alts
    [ do
        dest <- alts [ do eps; pure 0, do n <- fd; ws; pure n ]
        let mode = OpenForReading
        symbol '<'
        ws
        src <- redirectSource
        pure $ Redirect mode dest src
    , do
        dest <- alts [ do eps; pure 1, do n <- fd; ws; pure n ]
        mode <-
          alts [ do symbol  '>';  pure $ OpenForWriting Truncate
               , do keyword ">>"; pure $ OpenForWriting Append ]
        ws
        src <- redirectSource
        pure $ Redirect mode dest src
    ]

  redirectSource = alts [ FromPath <$> path, FromFD <$> fdRef ]
  fdRef = do symbol '&'; fd
  fd = FD <$> digit -- TODO: multi-digit file-desciptors
  path = Path.create <$> ident0

  word = alts [ Word <$> ident0
              , do keyword "$$"; pure DolDol ]

  keyword string = mapM_ symbol string

  ident0 = do
    x <- alts [alpha,numer]
    xs <- many (alts [alpha,numer,dash])
    pure (x : xs)

  digit = do c <- numer; pure (digitOfChar c)

  alpha = sat Char.isAlpha
  numer = sat Char.isDigit
  dash = sat (== '-')
  space = skip (sat Char.isSpace)

  symbol x = do t <-token; if t==x then pure () else fail
  sat pred = do c <- token; if pred c then pure c else fail

  ws = skipWhile space -- white*
  ws1 = do space; ws -- white+

  skip p = do _ <- p; eps
  eps = pure ()


digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

parseListSep :: Gram a -> Gram () -> Gram (a,[a])
parseListSep p sep = alts [
    do x <- p; sep; (x1,xs) <- parseListSep p sep; pure (x,x1:xs),
    do x <- p; pure (x,[])]
