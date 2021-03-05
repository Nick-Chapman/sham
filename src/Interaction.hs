module Interaction (Interaction(..)) where

import Misc (EOF(..))

data Interaction where
  I_Read :: (Either EOF String -> Interaction) -> Interaction
  I_Write :: String -> Interaction -> Interaction
  I_Trace :: String -> Interaction -> Interaction
  I_Halt :: Interaction
