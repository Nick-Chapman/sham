-- | An 'interaction' is the result of running a program on MeNicks.
module Interaction (Interaction(..),Prompt(..),OutMode(..)) where

import Misc (EOF(..))

data Interaction where
  I_Read :: Prompt -> (Either EOF String -> Interaction) -> Interaction
  I_Write :: OutMode -> String -> Interaction -> Interaction
  I_Trace :: String -> Interaction -> Interaction
  I_Halt :: Interaction

data Prompt = Prompt String | NoPrompt

data OutMode = Normal | StdErr -- TODO: rename Normal -> StdOut

instance Show OutMode where show = \case Normal -> "stdout"; StdErr -> "stderr"
