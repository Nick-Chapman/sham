module Top where

import FileSystem (fs0)
import Os (Prog,Interaction(..))
import System.IO (hFlush,stdout)
import qualified Bash (console)
import qualified Os (sim)

main :: IO ()
main = do
  putStrLn "*bash-sim*"
  runInteraction (Os.sim fs0 prog)

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
