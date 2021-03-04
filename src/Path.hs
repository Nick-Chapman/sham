module Path (Path,create,toString) where

newtype Path = Path { toString :: String }
  deriving (Eq,Ord)

instance Show Path where show = show . toString

create :: String -> Path
create name = Path { toString = name }
