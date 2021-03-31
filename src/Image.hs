-- | The initial file-system image, containing predefined 'binary' and 'data' files.
module Image (fs0,readme,days) where

import Bins (echo,env,cat,rev,grep,ls,mv,rm,ps,lsof,xargs,man,sum,type_)
import FileSystem (FileSystem)
import Prelude hiding (sum)
import Prog (BinaryMeta(..))
import Sham (sham)
import qualified File (createData,createProg)
import qualified FileSystem (create)
import qualified Path (create)

fs0 :: FileSystem
fs0 = FileSystem.create image where
  image =
    [ (Path.create p, File.createData lines) | (p,lines) <- scripts ] ++
    [ (Path.create p, File.createProg prog (BinaryMeta p)) | (p,prog) <- bins ]

  bins =
    [ ("echo",echo)
    , ("env",env)
    , ("cat",cat)
    , ("rev",rev)
    , ("grep",grep)
    , ("ls",ls)
    , ("mv",mv)
    , ("rm",rm)
    , ("ps",ps)
    , ("lsof",lsof)
    , ("sham",sham)
    , ("xargs",xargs)
    , ("man",man)
    , ("sum",sum)
    , ("type",type_)
    ]

  scripts  =
    [ ("README", readme)
    , ("days"  , days)
    , ("help"  , ["# Help the user!","cat README"])
    , ("cp"    , [
          "# cp SRC DEST: copy a file from SRC to DEST",
          "if $# != 2 echo $0 : takes two arguments >&2",
          "if $# != 2 exit",
          "cat $1 > $2"
          ])

    , ("yes"   , ["# Write an infinite stream of 'y's to stdout", "echo y","exec yes"])

    -- hidden scripts (path begins "."), for testing and experimentation

    , (".bomb"  , ["echo $$ >&2", ".bomb | .bomb"])
    , (".me"    , ["echo $$"])
    , (".countdown", [
          "# countdown N: generate decrementing sequence of numbers starting at N",
          "# if N is negative, the sequence will never stop",
          "if $# != 1 echo $0 : takes one argument >&2",
          "if $# != 1 exit",
          --"if $1=0 ps #debug",
          "if $1=0 exit",
          "echo $1",
          -- make it so countdown runs in constant number of processes...
          --"sum $1 -1 | xargs .countdown &" -- adding & here works
          "sum $1 -1 | (read v; exec .countdown $v) &" -- but read/exec is nicer than xargs
          ])

    , (".like-cat", [
          "# like-cat: this script should have the same behavior as cat",
          "read x",
          "echo $x",
          "like-cat"
          ])

    , (".head-1", [
          "# head-1: copy the first line on stdin to stdout",
          "read x",
          "echo $x"
          ])

    , (".make-head-N", [
          "# make-head: output a head-script for N determined by size of input",
          "read ignored",
          "echo .head-1",
          ".make-head-N"])

    , (".head", [
          "# head N: take first N line from stdin",
          "# if N is negative, this script will hang",
          "if $# != 1 echo $0 : takes one numeric argument >&2",
          "if $# != 1 exit",
          "echo > .tmp", -- TODO: fix ">>" so this is not necessary
          ".countdown $1 | .make-head-N >> .tmp",
          ".tmp"
          ])
{-
    , ("wip-head-needs-proc-sub", [
          "# head N: take first N line from stdin",
          "if $# != 1 echo head: takes one numeric argument >&2",
          "if $# != 1 exit",
          "if $1=0 exit",
          "read | cat",
          "exec head $(sum $1 -1)" -- TODO: support process substituion
          ])
-}
    ]

readme :: [String]
readme =
  [ "Welcome to *sham*."
  , "To discover available commands, run: 'ls | xargs type | grep Bin'"
  , "Type 'exit' or Ctrl-D to quit."
  ]

days :: [String]
days = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
