-- | A 'path' is a name used as key in file-system.
module Path (Path,create,toString,hidden) where

newtype Path = Path { toString :: String }
  deriving (Eq,Ord)

instance Show Path where show = show . toString

create :: String -> Path
create name = Path { toString = name }

hidden :: Path -> Bool
hidden (Path s) = case s of ('.':_) -> True; _ -> False
