module File (
  File,
  createProg, accessProg,
  createData, accessData,
  kind,
  ) where

import Prelude hiding (lines)
import Prog(Prog,FileKind(..),BinaryMeta(..))

createProg :: Prog () -> BinaryMeta -> File
createData :: [String] -> File
accessProg :: File -> Maybe (Prog ())
accessData :: File -> Maybe [String]
kind :: File -> FileKind

data File = Data [String] | Binary (Prog ()) BinaryMeta

createProg prog meta = Binary prog meta
accessProg = \case (Binary prog _) -> Just prog; Data{} -> Nothing

createData lines = Data lines
accessData = \case (Data lines) -> Just lines; Binary{} -> Nothing

kind = \case (Data _) -> K_Data; Binary _ meta -> K_Binary meta
