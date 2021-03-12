module Top (main) where

import Console (runInteraction)
import Image (fs0)
import Sham (sham)
import qualified MeNicks (run)
import qualified Tests (run)

main :: IO ()
main = do
  Tests.run (sham 1)
  putStrLn "Welcome to *sham*. You can type 'help'."
  Console.runInteraction (MeNicks.run fs0 (sham 1))
