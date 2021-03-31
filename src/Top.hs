-- | Entry point to the project. Run the regression tests, then start 'MeNicks'.
module Top (main) where

import System.Environment
import qualified AConsole (runInteraction)
import qualified Console (runInteraction)
import qualified Image (fs0)
import qualified MeNicks (start)
import qualified Tests (run)

main :: IO ()
main = do
  Tests.run
  let i = MeNicks.start Image.fs0
  useAsync >>= \case
    True -> do
      putStrLn "Welcome to *sham*. You can type 'help'. (Asynchronous console)"
      AConsole.runInteraction i
    False -> do
      putStrLn "Welcome to *sham*. You can type 'help'."
      Console.runInteraction i


useAsync :: IO Bool
useAsync = do
  getArgs >>= pure . \case
    ["async"] -> True
    _ -> False
