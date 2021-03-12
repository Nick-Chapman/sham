module Top (main) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import Image (fs0)
import Interaction (Interaction(..),Prompt(..),OutMode(..))
import Misc (EOF(..))
import Prog (Prog)
import qualified Data.Map.Strict as Map
import qualified Native (echo,cat,rev,grep,head,ls,ps,bins,xargs,man)
import qualified Prog (run)
import qualified Sham (Bins(..),sham,runCommand)
import qualified System.Console.ANSI as AN
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL
import qualified Tests (run)

main :: IO ()
main = do
  Tests.run (sham 1)
  putStrLn "Welcome to *sham*. You can type 'help'."
  runInteraction (Prog.run fs0 (sham 1))

sham :: Int -> Prog ()
sham level = Sham.sham level bins
  where
    bins = Sham.Bins binMap

    binMap :: Map String (Prog ())
    binMap = Map.fromList [ (name,prog) | (name,prog,_) <- table ]

    docMap :: Map String String
    docMap = Map.fromList [ (name,text) | (name,_,text) <- table ]

    table :: [(String,Prog (),String)]
    table =
      [ ("echo",Native.echo,
        "write given arguments to stdout")
      , ("sham",sham (level+1),
        "start a nested sham console")
      , ("cat",Native.cat,
        "write named files (or stdin in no files given) to stdout")
      , ("rev",Native.rev,
        "copy stdin to stdout, reversing each line")
      , ("grep",Native.grep,
        "copy lines which match the given pattern to stdout ")
      , ("head",Native.head,
        "copy just the first line on stdin to stdout, then exit")
      , ("ls",Native.ls,
        "list all files on the filesystem")
      , ("ps",Native.ps,
        "list all running process")
      , ("xargs",Native.xargs (Sham.runCommand bins),
        "concatenate lines from stdin, and pass as arguments to the given command")
      , ("bins",Native.bins (Map.keys binMap),
        "list builtin executables")
      , ("man",Native.man docMap,
         "list the manual entries for the given commands")
      ]

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
