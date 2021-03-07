module Interaction (Interaction(..),Prompt(..)) where

import Misc (EOF(..))

data Interaction where
  I_Read :: Prompt -> (Either EOF String -> Interaction) -> Interaction
  I_Write :: String -> Interaction -> Interaction
  I_Trace :: String -> Interaction -> Interaction
  I_Halt :: Interaction

data Prompt = Prompt String | NoPrompt
