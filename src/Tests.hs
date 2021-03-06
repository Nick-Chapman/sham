module Tests (run) where

import Testing (test)
import qualified Testing (run)

run :: IO ()
run = Testing.run $ do
  let words = ["one","two","three"]
  let rw = map reverse words

  test ["ls"] ["README","help","words"]

  test [] []
  test ["doh"] ["(stderr) no such path: doh"]

  test ["echo"] [""]
  test ["echo foo"] ["foo"]
  test ["echo foo", "echo bar"] ["foo","bar"]
  test ["echo foo bar"] ["foo bar"]
  test ["echo foo  bar"] ["foo bar"]
  test ["echo foo > x", "echo bar > x", "cat x"] ["bar"]
  test ["echo foo > x", "echo bar >> x", "cat x"] ["foo","bar"]
  test ["echo foo >&2"] ["(stderr) foo"]

  test ["cat words"] words
  test ["cat words words"] (words ++ words)
  test ["cat words","cat words"] (words ++ words)
  test ["cat words","echo foo"] (words ++ ["foo"])
  test ["cat < words"] words
  test ["rev < words"] rw
  test ["rev < words > rw", "cat rw"] rw

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
  test ["echo hey < words >&0"] ["(stderr) &1 not writable"]

  --test ["echo foo >&3"] ["(stderr) bad file descriptor: &3"] -- TODO: fail, get stray foo
  --test ["echo AA 3< words >&3"] [] -- TODO this crashes!

  test ["cat words &", "cat words"] ["one","two","one","three","two","three"]
  test ["cat words &", "echo FOO"] ["one","FOO","two","three"]

  test ["cat > x","echo OUT","echo ERR >&2","","x"] ["OUT","(stderr) ERR"]
  test ["cat > x","echo OUT","echo ERR >&2","",". x"] ["OUT","(stderr) ERR"]

  test ["exit"] []
  test ["cat > x","echo 1","exit","echo 2","","x"] ["1"]
  test ["echo exit > y","cat > x","echo 1","y","echo 2","","x"] ["1","2"]
  test ["echo exit > y","cat > x","echo 1",". y","echo 2","","x"] ["1"]
