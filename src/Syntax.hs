-- | Parser and AST for 'sham' scripts
module Syntax (
  Script(..), Redirect(..), RedirectSource(..), Pred(..), Word(..), Var(..),
  parseLine,
  ) where

import EarleyM (Gram,fail,alts,getToken,many,skipWhile,ParseError(..),Ambiguity(..),SyntaxError(..))
import Environment (Var(..))
import Prelude hiding (Word,read,fail)
import Prog (FD(..),OpenMode(..),WriteOpenMode(..))
import qualified Data.Char as Char
import qualified EarleyM as EM (parse,Parsing(..))

data Script -- TODO: loose Q prefix?
  = QNull
  | QSeq Script Script
  | QIf Pred Script Script
  | QShamError String
  | QEcho [Word]
  | QSetVar Var Word
  | QReadIntoVar Var
  | QExit
  | QExec Script
  | QInvoke Word [Word]
  | QSource Word [Word]
  | QPipeline [Script]
  | QBackGrounding Script
  | QRedirecting Script [Redirect] -- TODO: have just 1 redirect!
  deriving Show

data Pred
  = Eq Word Word
  | NotEq Word Word
  deriving Show

data Word
  = Word String
  | DollarHash
  | DollarN Int
  | DollarDollar
  | DollarName Var
  deriving Show

data Redirect = Redirect OpenMode FD RedirectSource
  deriving Show

data RedirectSource = FromPath Word | FromFD FD
  deriving Show


parseLine :: String -> Script
parseLine str = do
  case EM.parse (lang <$> getToken) str of
    EM.Parsing{EM.outcome} -> case outcome of
      Left pe -> QShamError $ prettyParseError str pe
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
                , do eps; pure QNull ]
    alts [eps,lineComment]
    pure res

  lineComment = do symbol '#'; skipWhile (skip token)

  script = alts [ pipeline, conditional, readIntoVar, setVar ]

  conditional = do
    keyword "if"
    ws1; p <- pred
    ws1; s <- step
    pure $ QIf p s QNull

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
    pure $ QReadIntoVar x

  setVar = do
    x <- varname
    keyword "=" -- no surrounding whitespace
    w <- word
    pure (QSetVar x w)

  pipeline = do
    x <- step
    xs <- many (do ws; symbol '|'; ws; step)
    m <- mode
    case xs of
      [] -> pure (m x)
      _ -> pure $ m $ QPipeline (x:xs)

  mode :: Gram (Script -> Script) =
    alts [ do eps; pure id,
           do ws; symbol '&'; pure QBackGrounding ]

  step = alts [run,exec,subshell]

  subshell = do
    symbol '('
    script <- sequence
    symbol ')'
    rs <- redirects
    pure $ (case rs of [] -> script; _-> QRedirecting script rs)

  sequence = do
    x1 <- script
    xs <- many (do ws; symbol ';'; ws; step)
    pure $ foldl QSeq x1 xs

  run = do
    thing <- command
    rs <- redirects
    pure $ (case rs of [] -> thing; _ -> QRedirecting thing rs)

  exec = do
    keyword "exec"
    ws1; thing <- command
    rs <- redirects
    pure $ QExec (case rs of [] -> thing; _ -> QRedirecting thing rs)

  command = alts [echo,exit,source,invocation]

  echo = do keyword "echo"; QEcho <$> words
  exit = do keyword "exit"; pure QExit

  source = do
    alts [keyword "source",keyword "."]
    ws1; w <- word
    ws <- words
    pure $ QSource w ws

  invocation = do
    com <- nonBuiltinWord
    args <- words
    pure $ QInvoke com args

  words = many (do ws1; word)

  builtinList = ["echo","exec","exit","read","source","."]

  nonBuiltinWord = do
    com <- word
    case com of Word w | w `elem` builtinList -> fail; _ -> pure com

  redirects = many (do ws1; redirect)
    -- TODO: Goal: allow just "ws" to separate args from redirects.
    -- Problem is that currently this causes ambiguity for examples such as:
    --  "echo foo1>xx"
    -- It should parse as:       "echo foo1 >xx"
    -- But we think it might be  "echo foo 1>xx"  !!

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
  fd = FD <$> digits

  word = alts [ Word <$> ident0
              , do keyword "$$"; pure DollarDollar
              , do keyword "$#"; pure DollarHash
              , do keyword "$"; DollarN <$> digits
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

  digits = digit >>= more
    where more n = alts [ pure n , do d <- digit; more (10*n+d)]

  digit = do c <- numer; pure (digitOfChar c)
    where digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

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
