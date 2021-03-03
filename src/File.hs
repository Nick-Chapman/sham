module File (File,create,empty,lines,append) where

import Prelude hiding (lines)

create :: [String] -> File
empty :: File
lines :: File -> [String]
append :: File -> String -> File

data File = File { reversedLines :: [String] }

create lines = File { reversedLines = reverse lines }
empty = create []
lines File{reversedLines=xs} = reverse xs
append File{reversedLines=xs} x = File { reversedLines = x:xs }
