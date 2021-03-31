-- | Play with async console...
module AConsole (runInteraction) where

import Control.Concurrent (MVar,forkIO,newEmptyMVar,putMVar,tryTakeMVar,threadDelay)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Interaction (Interaction(..),EOF(..),OutMode(..))
import qualified System.Console.ANSI as AN
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL

runInteraction :: Interaction -> IO ()
runInteraction i0 = do
  comm <- newComm
  HL.runInputT haskelineSettings $ do
    initialiseHistory
    putLine <- HL.getExternalPrint
    _ <- lift $ forkIO (supplyInteraction comm putLine i0)
    runPrompt comm

type In = Either EOF String
data Comm = Comm { user :: MVar In -- Prompt => Interaction
                 , done :: MVar () -- Interaction => Prompt
                 }

newComm :: IO Comm
newComm = do
  user <- newEmptyMVar
  done <- newEmptyMVar
  pure Comm { user, done }

supplyInteraction :: Comm -> (String -> IO ()) -> Interaction -> IO ()
supplyInteraction Comm{user,done} putLine i0 = loop i0
  where
    loop :: Interaction -> IO ()
    loop = \case
      I_Read _prompt f -> do -- TODO: dont ignore prompt
        threadDelay (1000 * 20) -- rate limit OS. TODO: what is the correct way of doing this?
        tryTakeMVar user >>= \m -> loop (f m)
      I_Write mode line i -> do
        putLine (colouring line)
        loop i
          where colouring = case mode of StdOut -> id; StdErr -> col AN.Red

      I_Trace mes i -> do
        putLine (col AN.Yellow mes)
        loop i
      I_Halt -> do
        putLine "*MeNicks* halted"
        putMVar done ()

runPrompt :: Comm -> HL.InputT IO ()
runPrompt Comm{user,done} = loop 0
  where
    loop :: Int -> HL.InputT IO ()
    loop n = do
      --lift $ threadDelay (1000 * 10) -- dont work; aim is to allow quick response to halt/done
      lift (tryTakeMVar done) >>= \case
        Just () ->
          pure ()
        Nothing -> do
          HL.getInputLine (col AN.Green ("prompt[" ++ show n ++ "]> ")) >>= \case
            Nothing -> do
              lift $ putMVar user (Left EOF)
              loop (n+1)
            Just line -> do
              when (line /= "") $ updateHistory line
              lift $ putMVar user (Right line)
              loop (n+1)

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
