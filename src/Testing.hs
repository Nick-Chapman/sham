module Testing (test,run) where

import Control.Monad (ap,liftM)
import Data.List (intercalate)
import Misc (EOF(..))
import qualified Bash (console)
import qualified Os (sim)
import FileSystem (fs0)
import Interaction (Interaction(..))

test :: [String] -> [String] -> Testing ()
test is xs = T1 (Test (Lines is) (Lines xs))

run :: Testing () -> IO ()
run testing = do
  bools <- sequence [ runTest i x | (i,x) <- zip [1..] (collect testing) ]
  let numTests = length bools
  let numPass = length [ () | res <- bools, res ]
  let numFail = numTests - numPass
  putStrLn $
    "ran " ++ show numTests ++ " tests; "
    ++ (if numFail > 0 then show numFail ++ " FAIL" else "all pass")

instance Functor Testing where fmap = liftM
instance Applicative Testing where pure = return; (<*>) = ap
instance Monad Testing where return = Ret; (>>=) = Bind

data Testing a where
  Ret :: a -> Testing a
  Bind :: Testing a -> (a -> Testing b) -> Testing b
  T1 :: Test -> Testing ()

collect :: Testing () -> [Test]
collect m = loop m $ \_ -> [] where
  loop :: Testing a -> (a -> [Test]) -> [Test]
  loop m k = case m of
    Ret a -> k a
    Bind m f -> loop m $ \a -> loop (f a) k
    T1 x -> x : k ()

data Test = Test Lines Lines

newtype Lines = Lines [String] deriving Eq
instance Show Lines where show (Lines xs) = intercalate "/" xs

instance Show Test where
  show (Test is xs) = "input: " ++ show is ++ "\n- expected: " ++ show xs

runTest :: Int -> Test -> IO Bool
runTest n t@(Test input expected) = do
  case bash input of
    Left (Unconsumed lines) -> do
      putStrLn $ "test #" ++ show n ++ " : " ++ show t
      putStrLn $ "- unconsumed: " ++ show lines
      pure False
    Right actual -> do
      if actual == expected then pure True else do
        putStrLn $ "test #" ++ show n ++ ", " ++ show t
        putStrLn $ "- actual: " ++ show actual
        pure False

bash :: Lines -> Either Unconsumed Lines
bash input = runInteraction (Os.sim fs0 Bash.console) input

runInteraction :: Interaction -> Lines -> Either Unconsumed Lines
runInteraction i (Lines xs0) = loop xs0 [] i where
  loop :: [String] -> [String] -> Interaction -> Either Unconsumed Lines
  loop xs ys = \case
    I_Read f -> do
      case xs of
        [] -> loop xs ys (f (Left EOF))
        line:xs -> loop xs ys (f (Right line))
    I_Write line i -> do
      loop xs (line:ys) i
    I_Trace _ i -> do
      loop xs ys i
    I_Halt -> do
      case xs of
        [] -> Right (Lines (reverse ys))
        _ -> Left (Unconsumed (Lines xs))

data Unconsumed = Unconsumed Lines
