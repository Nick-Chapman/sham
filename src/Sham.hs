
module Sham (sham) where

import Control.Monad (when)
import Data.Map (Map)
import EarleyM (Gram,fail,alts,getToken,many,skipWhile,ParseError(..),Ambiguity(..),SyntaxError(..))
import Interaction (Prompt(..))
import MeNicks (Prog,Pid(..),Command(..),SysCall(..),OpenMode(..),WriteOpenMode(..),FD(..))
import Misc (EOF(..),PipeEnds(..))
import Native (withOpen,err2)
import Prelude hiding (Word,read,fail)
import SysCall (BadFileDescriptor(..))
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified EarleyM as EM (parse,Parsing(..))
import qualified MeNicks (Prog(..))
import qualified Native (echo,cat,rev,grep,head,ls,ps,bins,xargs,man,readAll,read,write,checkNoArgs)
import qualified Path (create)

newtype Bins = Bins (Map String (Prog ()))

lookupBins :: Bins -> String -> Maybe (Prog ())
lookupBins (Bins m) k = Map.lookup k m

sham :: Int -> Prog ()
sham level = sham0 level bins
  where
    bins = Bins binMap

    binMap :: Map String (Prog ())
    binMap = Map.fromList [ (name,prog) | (name,prog,_) <- table ]

    docMap :: Map String String
    docMap = Map.fromList [ (name,text) | (name,_,text) <- table ]

    table :: [(String,Prog (),String)]
    table =
      [ ("echo",Native.echo,
        "write given arguments to stdout")
      , ("cat",Native.cat,
        "write named files (or stdin in no files given) to stdout")
      , ("rev",Native.rev,
        "copy stdin to stdout, reversing each line")
      , ("grep",Native.grep,
        "copy lines which match the given pattern to stdout ")
      , ("head",Native.head,
        "copy just the first line on stdin to stdout, then exit")
      , ("ls",Native.ls,
        "list all files on the filesystem")
      , ("ps",Native.ps,
        "list all running process")
      , ("sham",sham (level+1),
        "start a nested sham console")
      , ("xargs",Native.xargs (runCommand bins),
        "concatenate lines from stdin, and pass as arguments to the given command")
      , ("bins",Native.bins (Map.keys binMap),
        "list builtin executables")
      , ("man",Native.man docMap,
         "list the manual entries for the given commands")
      ]

sham0 :: Int -> Bins -> Prog ()
sham0 level bins = Native.checkNoArgs $ loop 1 where
  loop :: Int -> Prog ()
  loop n = do
    let prompt = "sham[" ++ show level ++ "." ++ show n ++ "]$ "
    Native.read (Prompt prompt) (FD 0) >>= \case
      Left EOF -> pure ()
      Right line -> do
        let script = parseLine line
        --MeNicks.Trace (show script)
        interpret bins script
        loop (n+1)


data Script
  = Null
  | ShamError String
  -- TODO: sequencing operator ";"
  | Pipe Script Script
  | BuiltinExit
  | BuiltinExec Word [Word]
  | BuiltinEcho [Word] -- run's in same process
  | BuiltinSource Word
  | Run Word [Word] [Redirect] WaitMode
  deriving Show

data Word = Word String | DollarDollar | DollarN Int
  deriving (Eq,Show)

data WaitMode = NoWait | Wait
  deriving (Eq,Show)

data Redirect
  = Redirect OpenMode FD RedirectSource
  deriving Show

data RedirectSource
  = FromPath Word
  | FromFD FD
  deriving Show

interpret :: Bins -> Script -> Prog ()
interpret bins = \case
  Null -> pure ()
  ShamError message -> err2 message
  BuiltinExit -> MeNicks.Exit
  BuiltinExec com args -> doExec bins com args
  BuiltinEcho args -> builtinEcho args
  BuiltinSource path -> evalWord path >>= runShamScript bins
  Run com args rs mode -> executeCommand bins com args rs mode
  Pipe script1 script2 -> pipe (interpret bins script1) (interpret bins script2)


pipe :: Prog () -> Prog () -> Prog ()
pipe prog1 prog2 = do
  PipeEnds{r,w} <- MeNicks.Call SysPipe ()
  let command = Command ("sham",[])
  spawn1 command (do -- TODO: dont loose the name of the actual pipe element
               dup2 (FD 1) w
               MeNicks.Call Close w
               MeNicks.Call Close r
               prog1
           ) $ \child1 -> do
    spawn1 command (do
                 dup2 (FD 0) r
                 MeNicks.Call Close w
                 MeNicks.Call Close r
                 prog2
             ) $ \child2 -> do
      MeNicks.Call Close w
      MeNicks.Call Close r
      MeNicks.Wait child1
      MeNicks.Wait child2

spawn1 :: Command -> Prog () -> (Pid -> Prog a) -> Prog a
spawn1 command child parent = do
  MeNicks.Fork >>= \case
    Nothing -> MeNicks.Exec command child
    Just pid -> parent pid


builtinEcho :: [Word] -> Prog ()
builtinEcho args = do
  args <- mapM evalWord args
  Native.write (FD 1) (unwords args)

executeCommand :: Bins -> Word -> [Word] -> [Redirect] -> WaitMode -> Prog ()
executeCommand bins com args rs mode = do
  MeNicks.Fork >>= \case
    Nothing -> do
      mapM_ execRedirect rs
      doExec bins com args
    Just pid -> case mode of
      Wait -> MeNicks.Wait pid
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
          Nothing -> runShamScript bins com
  MeNicks.Exec command prog

evalWord :: Word -> Prog String
evalWord = \case
  Word s -> pure s
  DollarDollar -> do Pid n <- MeNicks.MyPid; pure (show n)
  DollarN n -> do
    Command(com,args) <- MeNicks.Argv
    if n > length args then do err2 ("$" ++ show n ++ " undefined"); pure "" else
      pure $ (com:args)!!n

runShamScript :: Bins -> String -> Prog ()
runShamScript bins path = do
  lines <- do
    withOpen (Path.create path) OpenForReading $ \fd -> do
      Native.readAll fd
  sequence_ [ interpret bins (parseLine line) | line <- lines ]

execRedirect :: Redirect -> Prog ()
execRedirect r =
  case r of
    Redirect mode dest (FromPath path) -> do
      path <- evalWord path
      withOpen (Path.create path) mode $ \src -> do
        dup2 dest src
    Redirect _mode dest (FromFD src) -> do -- do we care what the mode is?
      dup2 dest src

dup2 :: FD -> FD -> Prog ()
dup2 d s = do
  MeNicks.Call Dup2 (d,s) >>= \case
    Left BadFileDescriptor -> do
      err2 $ "bad file descriptor: " ++ show s
      MeNicks.Exit
    Right () -> pure ()


----------------------------------------------------------------------
-- syntax...

parseLine :: String -> Script
parseLine str = do
  case EM.parse (lang <$> getToken) str of
    EM.Parsing{EM.outcome} -> case outcome of
      Left pe -> ShamError $ prettyParseError str pe -- TODO: improve parse error message for humans
      Right script -> script

prettyParseError :: String -> ParseError -> String
prettyParseError str = \case
  AmbiguityError (Ambiguity _ p1 p2) -> "ambiguous parse between positions " ++ show p1 ++ "--" ++ show p2
  SyntaxError (UnexpectedTokenAt p) -> "unexpected '" ++ char p ++ "' at position " ++ show p
  SyntaxError (UnexpectedEOF _) -> "unexpected end of line"
  SyntaxError (ExpectedEOF p) -> "expected EOF at position " ++ show p
  where char p = [str!!(p-1)]

lang :: Gram Char -> Gram Script
lang token = script where

  script = do
    ws
    res <- alts [ do res <- alts [ exit, exec, source, pipeline ]; ws; pure res
                , do eps; pure Null ]
    alts [eps,lineComment]
    pure res

  lineComment = do symbol '#'; skipWhile dot
  dot = do _ <- token; pure ()

  -- TODO: builtin exit/exec/source -- should be allowed a pipe-stage ? (like builtin-echo)
  exit = do keyword "exit"; pure BuiltinExit
  exec = do keyword "exec"; ws1; (com,args) <- parseListSep word ws1; pure $ BuiltinExec com args
  source = do keyword "."; ws1; path <- word; pure $ BuiltinSource path

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
    -- TODO: call fail before args for bettter err messages
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

  redirectSource = alts [ FromPath <$> word, FromFD <$> fdRef ]
  fdRef = do symbol '&'; fd
  fd = FD <$> digit -- TODO: multi-digit file-desciptors

  word = alts [ Word <$> ident0
              , do keyword "$$"; pure DollarDollar
              , do keyword "$"; DollarN <$> digit
              ]

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
