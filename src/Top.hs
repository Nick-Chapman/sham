-- | Entry point to the project. Run the regression tests, then start 'MeNicks'.
module Top (main) where

import Console (runInteraction)
import Image (fs0)
import qualified MeNicks (start)
import qualified Tests (run)

main :: IO ()
main = do
  Tests.run
  putStrLn "Welcome to *sham*. You can type 'help'."
  Console.runInteraction (MeNicks.start fs0)
