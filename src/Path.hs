module Path (Path,create,toString) where

newtype Path = Path { toString :: String }
  deriving (Eq,Ord)

create :: String -> Path
create name = Path { toString = name }
