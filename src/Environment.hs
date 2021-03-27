-- | Environment of bindings accessible to a process
module Environment (
  empty, Var(..), Environment, get, set, bindings,
  ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

newtype Var = Var String deriving (Eq,Ord)
data Environment = Environment (Map Var String)

instance Show Var where show (Var s) = s

empty :: Environment
get :: Environment -> Var -> Maybe String
set :: Environment -> Var -> String -> Environment
bindings :: Environment -> [(Var,String)]

empty = Environment Map.empty
get (Environment m) v = Map.lookup v m
set (Environment m) v s = Environment (Map.insert v s m)
bindings (Environment m) = Map.toList m
