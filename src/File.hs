module File (
  File,
  createProg, accessProg, Prog,
  createData, accessData,
  ) where

import Prelude hiding (lines)

data Prog = FILE_PROG -- dummy, TODO: use real Prog
  deriving Show

createProg :: Prog -> File
createData :: [String] -> File
accessProg :: File -> Maybe Prog
accessData :: File -> Maybe [String]

data File = Data [String] | Binary Prog

createProg prog = Binary prog
accessProg = \case (Binary prog) -> Just prog; Data{} -> Nothing

createData lines = Data lines
accessData = \case (Data lines) -> Just lines; Binary{} -> Nothing
