module Path (Path,create) where

newtype Path = Path { name :: String }
  deriving (Eq,Ord)

create :: String -> Path
create name = Path { name }
