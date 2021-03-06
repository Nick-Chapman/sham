module Tests (run) where

import Testing (test)
import qualified Testing (run)

run :: IO ()
run = Testing.run $ do
  let words = ["one","two","three"]
  test [] []
  test ["doh"] ["(stderr) no such path: doh"]
  test ["echo foo"] ["foo"]
  test ["echo foo >&2"] ["(stderr) foo"]
  test ["exit"] []
  test ["cat words"] words
  test ["rev < words"] (map reverse words)
  test ["echo foo > x", "echo bar > x", "cat x"] ["bar"]
  test ["echo foo > x", "echo bar >> x", "cat x"] ["foo","bar"]
  test ["rev 0> x"] ["(stderr) &0 not readable"]
  test ["echo hey < words >&0"] ["(stderr) &1 not writable"]
