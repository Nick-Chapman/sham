-- | Regression tests for 'sham' console interactions.
module Tests (run) where

import Data.List (isInfixOf)
import Testing (test)
import qualified FileSystem (ls)
import qualified Image (fs0,days,readme)
import qualified Path (toString,hidden)
import qualified Testing (run)

run :: IO ()
run = Testing.run $ do
  let days = Image.days
  let rw = map reverse days
  let merge xs ys = case xs of [] -> ys; x:xs -> x:merge ys xs
  let paths0 = [ Path.toString p | p <- FileSystem.ls Image.fs0, not (Path.hidden p) ]

  test ["echo foo"] ["foo"]
  test ["sham echo foo"] ["(stderr) cant open for reading: echo"]
  test ["sham -c echo foo"] ["foo"]

  test ["help"] Image.readme
  test [". help"] Image.readme
  test ["exec help"] Image.readme
  test ["sham help"] Image.readme
  test ["echo help | sham"] Image.readme
  test ["ls | grep lp | sham"] Image.readme
  test ["cat README"] Image.readme
  test ["sham cat README"] ["(stderr) cant open for reading: cat"]
  test ["sham -c cat README"] Image.readme
  test ["echo README | xargs cat"] Image.readme
  test ["cat README | xargs echo"] [ unwords Image.readme ]

  test ["*"] ["(stderr) unexpected '*' at position 1"]
  test ["*","echo ok"] ["(stderr) unexpected '*' at position 1","ok"]
  test ["README"] [
    "(stderr) unexpected '*' at position 12",
    "(stderr) unexpected ',' at position 31",
    "(stderr) unexpected ''' at position 6"]

  test ["ls"] paths0
  test ["sham ls"] ["(stderr) cant open for reading: ls"]
  test ["sham -c ls"] paths0
  test [". ls"] ["(stderr) cant open for reading: ls"]
  test ["echo ls | sham"] paths0
  test ["ls | xargs echo"] [unwords paths0]

  test ["echo cat | sham"] []

  test ["echo foo | rev"] ["oof"]
  test ["echo foo > x"," cat x | rev"] ["oof"]
  test ["echo rev > x","echo foo >> x"," cat x | sham"] ["oof"]
  test ["rev < days"] rw
  test ["cat days | rev"] rw
  test ["echo rev > x","cat days >> x","cat x | sham"] rw

  test ["rev < days > rw", "cat rw"] rw
  test ["cat days | rev | rev"] days
  test ["cat days | rev | rev | rev"] rw

  test ["echo foo | xargs ps"] ["(stderr) ps: takes no arguments"]

  test ["type"] ["(stderr) type: takes at least one argument"]
  test ["type ls help"] ["ls : Binary *ls*","help : Data/Script"]

  test ["man"] ["(stderr) man: takes at least one argument"]
  test ["man foo"] ["(stderr) man : no manual entry for 'foo'"]
  test ["man ps"] ["ps : list all running processes"]
  test ["echo ps | xargs man"] ["ps : list all running processes"]
  test ["echo ls ps | xargs man"] ["(stderr) man : no manual entry for 'ls ps'"]

  test ["mv"] ["(stderr) mv: takes two arguments"]
  test ["mv ps xx","type xx"] ["xx : Binary *ps*"]

  test ["if 2=3 echo foo"] []
  test ["if 2!=3 echo foo"] ["foo"]
  test ["if 4=4 echo foo"] ["foo"]
  test ["if 4!=4 echo foo"] []

  test ["sum 100 -1 0 200"] ["299"]
  test ["sum 100 -1 0 x200"] ["(stderr) sum: unable to convert 'x200' to a number","99"]

  test ["cat days | .head 1"] ["Monday"]
  test ["cat days | grep u | .head 1"] ["Tuesday"]
  test ["yes | .head 1", "echo woo hoo"] ["y","woo hoo"]
  test ["yes | .head 2", "echo woo hoo"] ["y","y","woo hoo"]
  test ["yes | .head 1", "yes | .head 1"] ["y","y"]

  test [".countdown"] ["(stderr) .countdown : takes one argument"]
  test [".countdown 0"] []
  test [".countdown 1"] ["1"]
  test [".countdown 3"] ["3","2","1"]
  test [".countdown -1 | .head-1"] ["-1"]

  test ["cat days | .head 3"] (take 3 days)
  test [".head 3 < days"] (take 3 days)
  test [".countdown -1 | .head 3"] ["-1","-2","-3"]

  test ["doh"] ["(stderr) no such executable: doh"]

  test ["echo $0"] ["sham"]
  test ["echo $1"] ["(stderr) $1 unbound",""]
  test ["echo $1","echo qaz"] ["(stderr) $1 unbound","","qaz"]
  test ["echo $1 qaz"] ["(stderr) $1 unbound"," qaz"]

  test ["cp README xx", "cat xx"] Image.readme
  test ["cp"] ["(stderr) cp : takes two arguments"]
  test ["cp foo"] ["(stderr) cp : takes two arguments"]
  test ["cp foo bar"] ["(stderr) no such path: foo"]

  test [] []
  test [""] []
  test ["echo"] [""]
  test ["echo foo", "echo bar"] ["foo","bar"]
  test ["echo foo bar"] ["foo bar"]
  test ["echo foo  bar"] ["foo bar"]
  test ["echo foo > x", "echo bar > x", "cat x"] ["bar"]
  test ["echo foo > x", "echo bar >> x", "cat x"] ["foo","bar"]
  test ["echo foo >&2"] ["(stderr) foo"]

  test ["cat days"] days
  test ["cat days days"] (days ++ days)
  test ["cat days","cat days"] (days ++ days)
  test ["cat days","echo foo"] (days ++ ["foo"])
  test ["cat < days"] days

  test ["*"] ["(stderr) unexpected '*' at position 1"]
  test ["."] ["(stderr) source takes at least one argument"]
  test ["exit nope"] ["(stderr) exit takes no args"]

  test ["rev nope"] ["(stderr) rev: takes no arguments"]
  test ["rev nope < days"] ["(stderr) rev: takes no arguments"]
  test ["ls nope"] ["(stderr) ls: takes no arguments, or a single '-a'"]
  test ["ps nope"] ["(stderr) ps: takes no arguments"]

  test ["echo doh > x","echo echo foo >> x","x"] ["(stderr) no such executable: doh","foo"]
  test ["echo doh > x","echo echo foo >> x","x > hide"] ["(stderr) no such executable: doh"]
  test ["echo doh > x","echo echo foo >> x","x 2> hide"] ["foo"]
  test ["echo doh > x","echo echo foo >> x","x 2>&1 > hide"] ["no such executable: doh"]
  test ["echo doh > x","echo echo foo >> x","x > hide 2>&1"] []
  test ["echo doh > x","echo echo foo >> x","x 3>&2 2>&1 1>&3"]
    ["no such executable: doh","(stderr) foo"]
  test ["echo doh > x","echo echo foo >> x","x 3>&1 1>&2 2>&3"]
    ["no such executable: doh","(stderr) foo"]

  test ["rev 0> x"] ["(stderr) &0 not readable"]
  test ["echo hey < days >&0"] ["(stderr) &1 not writable"]

  test ["echo AA 4< days >&4"] ["(stderr) &1 not writable"]
  test ["doh 4< days"] ["(stderr) no such executable: doh"]
  test ["doh 4< days 2>&4"] [] -- redirecting stderr to unwritable FD looses error

  test ["cat days &", "cat days"] (take 1 days ++ merge (drop 1 days) days) -- rather fragile

  test ["cat > x","echo OUT","echo ERR >&2","","x"] ["OUT","(stderr) ERR"]
  test ["cat > x","echo OUT","echo ERR >&2","",". x"] ["OUT","(stderr) ERR"]

  test ["exit"] []
  test ["cat > x","echo 1","exit","echo 2","","x"] ["1"]
  test ["echo exit > y","cat > x","echo 1","y","echo 2","","x"] ["1","2"]
  test ["echo exit > y","cat > x","echo 1",". y","echo 2","","x"] ["1"]

  test ["cat days | grep u"] [ d | d <- days, "u" `isInfixOf` d ]
  test ["grep"] ["(stderr) grep: takes a single argument"]

  test ["echo my pid is $$"] ["my pid is 4"]
  test [".me"] ["5"]
  test ["exec .me"] ["4"]
  test ["cat .me > a","echo .me >> a","a",".me"] ["7","8","9"]
  test ["cat .me > a","echo exec .me >> a","a",".me"] ["7","7","8"]

  test ["(echo foo; echo bar)"] ["foo","bar"]
  test ["(echo foo; echo bar) | rev"] ["oof","rab"]
  test ["(ls;cat days) | grep es"] ["yes","Tuesday","Wednesday"]

  -- backgrounding is massivley elayed since tty rework
  -- test ["help &"] Image.readme
  -- test ["cat days &", "echo FOO"] ("FOO" : days)

  test ["ps"] ["[1] init","[2] tty-stderr","[3] tty-stdout","[4] sham","[5] ps"]

  -- tests that show ps are to fragile
  --test ["(ls;ps)"] (paths0 ++ ["[1] tty-monitor-stdout","[2] sham","[4] ps"])
  --test ["(ls;ps) >x","cat x"] (paths0 ++ ["[1] tty-monitor-stdout","[2] sham","[3] ps"])

  -- lsof output is too fragile
  --test ["lsof | cat"] ["[3] (lsof) &1 Write:pipe1", "[4] (cat) &0 Read:pipe1"]
  --test ["ls | grep es","lsof"] ["yes"]
  --test ["(ls;ls) | grep es","lsof"] ["yes","yes"]

  -- output truncated following tty rework
  -- test ["exec ps"] ["[1] tty-monitor-stdout","[2] ps"]
  -- test ["exec ls"] paths0

  -- TODO: rewrite of bash interpreter has broken these...
  --test ["exec 2>e","foo","bar","cat e"] ["no such executable: foo", "no such executable: bar"]
  --test ["exec >&2", "echo foo"] ["(stderr) foo"]


  --test ["echo foo >&0"] [] -- TODO: crashes because provokes old tty code
  test ["echo foo >&1"] ["foo"]
  test ["echo foo >&2"] ["(stderr) foo"]
  test ["echo foo >&3"] ["(stderr) bad file descriptor: &3"]
  test ["echo foo >&7"] ["(stderr) bad file descriptor: &7"]


  test ["env"] ["Version=MeNicks-0.1","prefix=sham"]
  test ["foo=123","echo $foo"] ["123"]
  test ["foo=123","bar=456","echo $foo $bar"] ["123 456"]
  test ["foo=ls","$foo"] paths0
