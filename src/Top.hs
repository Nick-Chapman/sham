-- | Entry point to the project. Run regression tests, then start a 'sham' console on 'MeNicks'.
module Top (main) where

import Console (runInteraction)
import Image (fs0)
import Sham (shamConsole)
import qualified MeNicks (run)
import qualified Tests (run)

main :: IO ()
main = do
  Tests.run (shamConsole 1)
  putStrLn "Welcome to *sham*. You can type 'help'."
  Console.runInteraction (MeNicks.run fs0 (shamConsole 1))
