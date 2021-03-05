module Top where

import Os (Prog,Interaction(..))
import qualified Os (sim)
import qualified Bash (console)
import System.IO (hFlush,stdout)

main :: IO ()
main = do
  putStrLn "*bash-sim*"
  runInteraction (Os.sim prog)

prog :: Prog ()
prog = Bash.console

runInteraction :: Interaction -> IO ()
runInteraction = loop where
  loop :: Interaction -> IO ()
  loop = \case
    ReadLine f -> do
      putStr $ "> "
      hFlush stdout
      line <- getLine
      --putStr $ "[read:" ++ line ++ "]"
      --let res = if line == "" then Left EOF else Right line -- hack for Ctr-D
      let res = Right line
      loop (f res)

    WriteLine line i -> do
      putStrLn line
      loop i
    TraceLine s i -> do
      putStrLn $ "*trace[" ++ s ++ "]*"
      loop i
    Halt -> do
      putStrLn "*halt*"
