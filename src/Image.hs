module Image (fs0,readme,days) where

import FileSystem (FileSystem)
import Prog (BinaryMeta(..))
import qualified File (createData,createProg)
import qualified FileSystem (create)
import qualified Native
import qualified Path (create)
import qualified Sham (runCommand,sham)

fs0 :: FileSystem
fs0 = FileSystem.create image where
  image =
    [ (Path.create p, File.createData lines) | (p,lines) <- scripts ] ++
    [ (Path.create p, File.createProg prog (BinaryMeta p)) | (p,prog) <- bins ]

  bins =
    [ ("echo",Native.echo)
    , ("cat",Native.cat)
    , ("rev",Native.rev)
    , ("grep",Native.grep)
    , ("ls",Native.ls)
    , ("ps",Native.ps)
    , ("sham",Sham.sham)
    , ("xargs",Native.xargs Sham.runCommand)
    , ("man",Native.man)
    , ("sum",Native.sum)
    , ("type",Native.type_)
    ]

  scripts  =
    [ ("README", readme)
    , ("days"  , days)
    , ("help"  , ["cat README"])
    , ("yes"   , ["echo yes","exec yes"])
    , ("bomb"  , ["echo $$ >&2", "bomb | bomb"])
    , ("me"    , ["echo $$"])
    , ("cp"    , ["# cp SRC DEST: copy a file from SRC to DEST","cat $1 > $2"])

    , ("countdown", [
          "# countdown N: generate decrementing sequence of numbers starting at N",
          "# if N is negative, the sequence will never stop",
          "if $# != 1 echo countdown: takes one argument >&2",
          "if $# != 1 exit",
          --"if $1=0 ps #debug",
          "if $1=0 exit",
          "echo $1",
          "sum $1 -1 | xargs countdown"
          ])

    , ("like-cat", [
          "# like-cat: this script should have the same behavior as cat",
          "read x",
          "echo $x",
          "like-cat"
          ])

    , ("head-1", [
          "# head-1: copy the first line on stdin to stdout",
          "read x",
          "echo $x"
          ])

    , ("make-head-N", [
          "# make-head: output a head-script for N determined by size of input",
          "read ignored",
          "echo head-1",
          "make-head-N"])

    , ("head", [
          "# head N: take first N line from stdin",
          "# if N is negative, this script will hang",
          "if $# != 1 echo head: takes one numeric argument >&2",
          "if $# != 1 exit",
          "echo > tmp", -- TODO: fix ">>" so this is not necessary
          "countdown $1 | make-head-N >> tmp",
          "tmp"
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
  , "Some available commands include: echo, cat, rev, grep, ls, man, ps, xargs."
  , "Type 'exit' or Ctrl-D to quit."
  ]

days :: [String]
days = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
