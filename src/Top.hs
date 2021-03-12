module Top (main) where

import Console (runInteraction)
import Data.Map (Map)
import Image (fs0)
import Prog (Prog)
import qualified Data.Map.Strict as Map
import qualified Native (echo,cat,rev,grep,head,ls,ps,bins,xargs,man)
import qualified Prog (run)
import qualified Sham (Bins(..),sham,runCommand)
import qualified Tests (run)

main :: IO ()
main = do
  Tests.run (sham 1)
  putStrLn "Welcome to *sham*. You can type 'help'."
  Console.runInteraction (Prog.run fs0 (sham 1))

sham :: Int -> Prog ()
sham level = Sham.sham level bins
  where
    bins = Sham.Bins binMap

    binMap :: Map String (Prog ())
    binMap = Map.fromList [ (name,prog) | (name,prog,_) <- table ]

    docMap :: Map String String
    docMap = Map.fromList [ (name,text) | (name,_,text) <- table ]

    table :: [(String,Prog (),String)]
    table =
      [ ("echo",Native.echo,
        "write given arguments to stdout")
      , ("sham",sham (level+1),
        "start a nested sham console")
      , ("cat",Native.cat,
        "write named files (or stdin in no files given) to stdout")
      , ("rev",Native.rev,
        "copy stdin to stdout, reversing each line")
      , ("grep",Native.grep,
        "copy lines which match the given pattern to stdout ")
      , ("head",Native.head,
        "copy just the first line on stdin to stdout, then exit")
      , ("ls",Native.ls,
        "list all files on the filesystem")
      , ("ps",Native.ps,
        "list all running process")
      , ("xargs",Native.xargs (Sham.runCommand bins),
        "concatenate lines from stdin, and pass as arguments to the given command")
      , ("bins",Native.bins (Map.keys binMap),
        "list builtin executables")
      , ("man",Native.man docMap,
         "list the manual entries for the given commands")
      ]
