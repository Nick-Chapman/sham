-- | Entry point to the project. Run the regression tests, then start 'MeNicks'.
module Top (main) where

import qualified AConsole (runInteraction)
import qualified Console (runInteraction)
import qualified Image (fs0)
import qualified MeNicks (start)
import qualified Tests (run)

main :: IO ()
main = do
  Tests.run
  putStrLn "Welcome to *sham*. You can type 'help'."
  runInteraction (MeNicks.start Image.fs0)
    where
      runInteraction = AConsole.runInteraction -- WIP: async console
      _runInteraction = Console.runInteraction -- old console
