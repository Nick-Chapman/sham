module File (
  File,
  createProg, accessProg,
  createData, accessData,
  ) where

import Prelude hiding (lines)
import Prog(Prog)

createProg :: Prog () -> File
createData :: [String] -> File
accessProg :: File -> Maybe (Prog ())
accessData :: File -> Maybe [String]

data File = Data [String] | Binary (Prog ())

createProg prog = Binary prog
accessProg = \case (Binary prog) -> Just prog; Data{} -> Nothing

createData lines = Data lines
accessData = \case (Data lines) -> Just lines; Binary{} -> Nothing
