module Image (fs0,readme,days) where

import FileSystem (FileSystem)
import qualified FileSystem (create)
import qualified Path (create)
import qualified File (create)

fs0 :: FileSystem
fs0 = FileSystem.create [ (Path.create p, File.create lines) | (p,lines) <- image ] where
  image =
    [ ("README", readme)
    , ("days"  , days)
    , ("help"  , ["cat README"])
    , ("yes"   , ["echo yes","exec yes"])
    , ("bomb"  , ["echo $$ >&2", "bomb | bomb"])
    , ("me"    , ["echo $$"])
    , ("cp"    , ["# cp SRC DEST: copy a file from SRC to DEST","cat $1 > $2"])
    ]

readme :: [String]
readme =
  [ "Welcome to *sham*."
  , "Some available commands include: bins, echo, cat, rev, grep, ls, man, ps, xargs."
  , "Type 'exit' or Ctrl-D to quit."
  ]

days :: [String]
days = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
