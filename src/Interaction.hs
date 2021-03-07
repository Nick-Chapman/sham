module Interaction (Interaction(..),Prompt(..)) where

import Misc (EOF(..))

data Interaction where
  I_Read :: Prompt -> (Either EOF String -> Interaction) -> Interaction
  -- TODO: distinguish normal/error Write (to allow console to colour stderr)
  I_Write :: String -> Interaction -> Interaction
  I_Trace :: String -> Interaction -> Interaction
  I_Halt :: Interaction

data Prompt = Prompt String | NoPrompt
