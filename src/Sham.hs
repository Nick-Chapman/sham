-- | 'sham' is a shell-style command interpreter which runs on MeNicks.
module Sham (sham) where

import EarleyM (Gram,fail,alts,getToken,many,skipWhile,ParseError(..),Ambiguity(..),SyntaxError(..))
import Interaction (Prompt(..))
import Lib (loadFile,read)
import Misc (EOF(..))
import Prelude hiding (Word,read,fail)
import Prog (Prog,FD(..),Command(..),OpenMode(..),WriteOpenMode(..))
import Script (Script(..),Step(..),Invocation(..),WaitMode(..),Redirect(..),RedirectSource(..),Pred(..),Word(..),Var(..))
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified EarleyM as EM (parse,Parsing(..))
import qualified Prog --(Prog(Argv,MyPid))
import qualified Script (runScript,Env(..))

sham :: Prog ()
sham = do
  Command(_sham,args) <- Prog.Argv
  case args of

    "-c":rest -> do
      let script = parseLine (unwords rest)
      pid <- Prog.MyPid
      let com = "sham"
      let args = []
      let bindings = Map.empty
      let env = Script.Env { pid, com, args, bindings, shamParser}
      Script.runScript env script

    path:args -> do
      lines <- loadFile path
      let script = shamParser lines
      pid <- Prog.MyPid
      let com = path
      let bindings = Map.empty
      let env = Script.Env { pid, com, args, bindings, shamParser}
      Script.runScript env script

    [] -> loop 1 where
      loop :: Int -> Prog ()
      loop n = do
        let level :: Int = 1 -- TODO: get from env
        let prompt = "sham[" ++ show level ++ "." ++ show n ++ "]$ "
        read (Prompt prompt) (FD 0) >>= \case
          Left EOF -> pure ()
          Right line -> do
            let script = parseLine line
            pid <- Prog.MyPid
            let com = "sham"
            let args = []
            let bindings = Map.empty
            let env = Script.Env { pid, com, args, bindings, shamParser}
            Script.runScript env script
            loop (n+1)


shamParser :: [String] -> Script
shamParser lines = foldl Seq Null (map parseLine lines)

parseLine :: String -> Script
parseLine str = do
  case EM.parse (lang <$> getToken) str of
    EM.Parsing{EM.outcome} -> case outcome of
      Left pe -> ShamError $ prettyParseError str pe
      Right script -> script

-- TODO: when parsing a script it would be nice to see the line number for a parse error
prettyParseError :: String -> ParseError -> String
prettyParseError str = \case
  AmbiguityError (Ambiguity _ p1 p2) -> "ambiguous parse between positions " ++ show p1 ++ "--" ++ show p2
  SyntaxError (UnexpectedTokenAt p) -> "unexpected '" ++ char p ++ "' at position " ++ show p
  SyntaxError (UnexpectedEOF _) -> "unexpected end of line"
  SyntaxError (ExpectedEOF p) -> "expected EOF at position " ++ show p
  where char p = [str!!(p-1)]

lang :: Gram Char -> Gram Script
lang token = script0 where

  script0 = do
    ws
    res <- alts [ do res <- script; ws; pure res
                , do eps; pure Null ]
    alts [eps,lineComment]
    pure res

  lineComment = do symbol '#'; skipWhile (skip token)

  script = alts [ pipeline, conditional, readIntoVar ]

  conditional = do
    keyword "if"
    ws1; p <- pred
    ws1; s <- step
    pure $ If p (Invoke1 s Wait) Null

  pred = alts [equal,notEqual]

  equal = do
    x1 <- word
    ws; keyword "="; ws
    x2 <- word
    pure (Eq x1 x2)

  notEqual = do
    x1 <- word
    ws; keyword "!="; ws
    x2 <- word
    pure (NotEq x1 x2)

  readIntoVar = do
    keyword "read"
    ws1; x <- varname
    pure $ ReadIntoVar x

  pipeline = do
    (x,xs) <- parseListSep step (do ws; symbol '|'; ws)
    m <- mode
    case xs of
      [] -> pure $ Invoke1 x m
      _ -> pure $ Pipeline (x:xs) m

  mode =
    alts [ do eps; pure Wait,
           do ws; symbol '&'; pure NoWait ]

  step = alts [run,subshell]

  subshell = do
    symbol '('
    script <- sequence
    symbol ')'
    rs <- redirects
    pure $ XSubShell script rs

  sequence = do
    (x1,xs) <- parseListSep script (do ws; symbol ';'; ws)
    pure $ foldl Seq x1 xs

  {-run = do
    (com,args) <- parseListSep word ws1
    case com of Word "read" -> fail; _ -> pure ()
    rs <- redirects
    pure $ Run (Invocation com args) rs
-}
  run = do
    thing <- invocation
    rs <- redirects
    pure $ Run thing rs

  invocation = do
    (com,args) <- parseListSep word ws1
    case com of Word "read" -> fail; _ -> pure ()
    pure $ Invocation com args


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
        dest <- alts [ do eps; pure 0, do n <- fd; pure n ]
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
              , do keyword "$#"; pure DollarHash
              , do keyword "$"; DollarN <$> digit
              , do keyword "$"; DollarName <$> varname
              ]

  keyword string = mapM_ symbol string

  varname = Var <$> do
    x <- alpha
    xs <- many (alts [alpha,numer])
    pure (x : xs)

  ident0 = do
    x <- alts [alpha,numer,dash,dot,colon]
    xs <- many (alts [alpha,numer,dash,dot,colon])
    pure (x : xs)

  digit = do c <- numer; pure (digitOfChar c)

  alpha = sat Char.isAlpha
  numer = sat Char.isDigit
  dash = sat (== '-')
  dot = sat (== '.')
  colon = sat (== ':')
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
