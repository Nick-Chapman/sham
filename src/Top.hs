module Top (main) where

import Console (runInteraction)
import Image (fs0)
import Sham (sham)
import qualified Prog (runMeNicks)
import qualified Tests (run)

main :: IO ()
main = do
  Tests.run (sham 1)
  putStrLn "Welcome to *sham*. You can type 'help'."
  Console.runInteraction (Prog.runMeNicks fs0 (sham 1))
