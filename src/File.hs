module File (File,empty,lines,append) where

import Prelude hiding (lines)

data File

empty :: File
lines :: File -> [String]
append :: File -> String -> File

(empty,lines,append) = undefined
