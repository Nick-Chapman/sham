module Tests (run) where

import Data.List (isInfixOf)
import Testing (test)
import Prog (Prog)
import qualified Testing (run)
import qualified FileSystem (days,readme)

run :: Prog () -> IO ()
run console = Testing.run console $ do
  let days = FileSystem.days
  let rw = map reverse days
  let merge xs ys = case xs of [] -> ys; x:xs -> x:merge ys xs
  let paths0 = ["README","days","help","y"]

  test ["ls"] paths0
  test ["help"] FileSystem.readme
  test ["help &"] FileSystem.readme
  test ["doh"] ["(stderr) no such path: doh"]

  test [] []
  test ["echo"] [""]
  test ["echo foo"] ["foo"]
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
  test ["rev < days"] rw
  test ["rev < days > rw", "cat rw"] rw

  test ["."] ["(stderr) source (.): takes exactly one argument"]
  test ["exit nope"] ["(stderr) exit: takes no arguments"]
  test ["rev nope"] ["(stderr) rev: takes no arguments"]
  test ["rev nope < days"] ["(stderr) rev: takes no arguments"]
  test ["ls nope"] ["(stderr) ls: takes no arguments"]
  test ["ps nope"] ["(stderr) ps: takes no arguments"]

  test ["echo doh > x","echo echo foo >> x","x"] ["(stderr) no such path: doh","foo"]
  test ["echo doh > x","echo echo foo >> x","x > hide"] ["(stderr) no such path: doh"]
  test ["echo doh > x","echo echo foo >> x","x 2> hide"] ["foo"]
  test ["echo doh > x","echo echo foo >> x","x 2>&1 > hide"] ["no such path: doh"]
  test ["echo doh > x","echo echo foo >> x","x > hide 2>&1"] []
  test ["echo doh > x","echo echo foo >> x","x 3>&2 2>&1 1>&3"]
    ["no such path: doh","(stderr) foo"]
  test ["echo doh > x","echo echo foo >> x","x 3>&1 1>&2 2>&3"]
    ["no such path: doh","(stderr) foo"]

  test ["rev 0> x"] ["(stderr) &0 not readable"]
  test ["echo hey < days >&0"] ["(stderr) &1 not writable"]

  test ["echo foo >&3"] ["(stderr) bad file descriptor: &3"]
  test ["echo AA 3< days >&3"] ["(stderr) bad file descriptor: &3"] -- ?? file-opens on fd-3
  test ["echo AA 4< days >&4"] ["(stderr) &1 not writable"]
  test ["doh 4< days"] ["(stderr) no such path: doh"]
  test ["doh 4< days 2>&4"] [] -- redirecting stderr to unwritable FD looses error

  test ["cat days &", "cat days"] (head days : merge (tail days) days)
  test ["cat days &", "echo FOO"] (head days : "FOO" : tail days)

  test ["cat > x","echo OUT","echo ERR >&2","","x"] ["OUT","(stderr) ERR"]
  test ["cat > x","echo OUT","echo ERR >&2","",". x"] ["OUT","(stderr) ERR"]

  test ["echo foo | rev"] ["oof"]
  test ["cat days | rev"] rw
  test ["cat days | rev | rev"] days
  test ["cat days | rev | rev | rev"] rw

  test ["exit"] []
  test ["cat > x","echo 1","exit","echo 2","","x"] ["1"]
  test ["echo exit > y","cat > x","echo 1","y","echo 2","","x"] ["1","2"]
  test ["echo exit > y","cat > x","echo 1",". y","echo 2","","x"] ["1"]

  test ["ps"] ["[1] init","[2] ps"]
  test ["bins"] ["bash","bins","cat","echo","grep","head","ls","man","ps","rev","xargs"]
  test ["ls | xargs echo"] [unwords paths0]
  test ["cat days | grep u"] [ d | d <- days, "u" `isInfixOf` d ]
  test ["grep"] ["(stderr) grep: takes a single argument"]
  test ["cat days | head"] ["Monday"]
  test ["cat days | grep u | head"] ["Tuesday"]
