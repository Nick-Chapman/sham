module Top (main) where

import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import FileSystem (fs0)
import Interaction (Interaction(..),Prompt(..))
import Misc (EOF(..))
import Os (Prog)
import qualified Bash (Bins(..),console,bash)
import qualified Data.Map.Strict as Map
import qualified Native (echo,cat,ls,ps,rev,xargs,builtins)
import qualified Os (sim)
import qualified System.Console.ANSI as AN
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL
import qualified Tests (run)

main :: IO ()
main = do
  Tests.run console
  putStrLn "*bash-sim* (try typing help)"
  runInteraction (Os.sim fs0 console)

console :: Prog ()
console = Bash.console bins
  where
    binMap :: Map String ([String] -> Prog ())
    binMap = Map.fromList
      [ ("echo",Native.echo)
      , ("cat",Native.cat)
      , ("rev",Native.rev)
      , ("ls",Native.ls)
      , ("ps",Native.ps)
      , ("xargs",Native.xargs (Bash.bash bins))
      , ("builtins",Native.builtins (Map.keys binMap)) -- TODO: rename bins
      -- TODO: add bash
      ]
    bins = Bash.Bins binMap

runInteraction :: Interaction -> IO ()
runInteraction i0 = do
  HL.runInputT haskelineSettings $ do
    initialiseHistory
    loop (1::Int) i0
  where
    loop n = \case
      I_Read pM f -> do
        case pM of

          NoPrompt -> do
            HL.getInputLine "(more...) " >>= \case
              Nothing -> loop n (f (Left EOF))
              Just line -> do
                loop n (f (Right line))

          Prompt prompt -> do
            HL.getInputLine (col AN.Green (show n ++ prompt)) >>= \case
              Nothing -> loop n (f (Left EOF))
              Just line -> do
                updateHistory line
                loop (n+1) (f (Right line))

      I_Write line i -> do
        lift $ putStrLn line
        loop n i

      I_Trace mes i -> do
        lift $ putStrLn (col AN.Yellow mes)
        loop n i

      I_Halt -> do
        lift $ putStrLn "*halt*"


col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]


haskelineSettings :: HL.Settings IO
haskelineSettings = HL.defaultSettings {HL.autoAddHistory = False}


-- keep history in opposite order from HL standard (newest at end of file)

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

revHistory :: HL.History -> HL.History
revHistory = foldl (flip HL.addHistory) HL.emptyHistory . HL.historyLines
