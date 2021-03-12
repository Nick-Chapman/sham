module Console (runInteraction) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Interaction (Interaction(..),Prompt(..),OutMode(..))
import Misc (EOF(..))
import qualified System.Console.ANSI as AN
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL

runInteraction :: Interaction -> IO ()
runInteraction i0 = do
  HL.runInputT haskelineSettings $ do
    initialiseHistory
    loop i0
  where
    loop = \case
      I_Read pM f -> do
        case pM of
          NoPrompt -> do
            HL.getInputLine "(more...) " >>= \case
              Nothing -> loop (f (Left EOF))
              Just line -> do
                loop (f (Right line))
          Prompt prompt -> do
            HL.getInputLine (col AN.Green prompt) >>= \case
              Nothing -> loop (f (Left EOF))
              Just line -> do
                when (line /= "") $ updateHistory line
                loop (f (Right line))
      I_Write mode line i -> do
        lift $ putStrLn (colouring line)
        loop i
          where colouring = case mode of Normal -> id; StdErr -> col AN.Red
      I_Trace mes i -> do
        lift $ putStrLn (col AN.Yellow mes)
        loop i
      I_Halt -> do
        lift $ putStrLn "*MeNicks* halted"

col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]

haskelineSettings :: HL.Settings IO
haskelineSettings = HL.defaultSettings { HL.autoAddHistory = False }

histFile :: String
histFile = ".history"

initialiseHistory :: HL.InputT IO ()
initialiseHistory = do
  history <- lift (HL.readHistory histFile)
  HL.putHistory (revHistory history)

updateHistory :: String -> HL.InputT IO ()
updateHistory line = do
  HL.modifyHistory (HL.addHistory line)
  history <- HL.getHistory
  lift (HL.writeHistory histFile (revHistory history))

-- keep history in opposite order from HL standard (newest at end of file)
revHistory :: HL.History -> HL.History
revHistory = foldl (flip HL.addHistory) HL.emptyHistory . HL.historyLines
