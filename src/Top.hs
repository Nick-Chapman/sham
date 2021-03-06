module Top where

import FileSystem (fs0)
import Interaction (Interaction(..))
import Os (Prog)
import System.IO (hFlush,stdout)
import qualified Bash (console)
import qualified Os (sim)
import qualified Tests (run)

main :: IO ()
main = do
  putStrLn "*bash-sim*"
  Tests.run
  runInteraction (Os.sim fs0 prog)

prog :: Prog ()
prog = Bash.console

-- TODO: use haskeline so get a nice console, with history!
runInteraction :: Interaction -> IO ()
runInteraction = loop where
  loop :: Interaction -> IO ()
  loop = \case

    I_Read f -> do
      putStr $ "> "
      hFlush stdout
      line <- getLine
      let res = Right line
      loop (f res)

    I_Write line i -> do
      putStrLn line
      loop i
    I_Trace s i -> do
      putStrLn $ "*trace[" ++ s ++ "]*"
      loop i
    I_Halt -> do
      putStrLn "*halt*"
