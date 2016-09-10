-- Modification (to introduce bug) of an example in Parallel and
-- Concurrent Programming in Haskell, chapter 7.
module Examples.Logger where

import Control.Concurrent.Classy
import Data.Functor (void)
import System.Random (mkStdGen)
import Test.DejaFu hiding (MemType(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (test)
import Test.HUnit.DejaFu

tests :: [Test]
tests = hUnitTestToTests . test $ testDejafus raceyLogger
  [ ("allowed", validResult)
  , ("correct occurs", isGood)
  , ("bug exists", isBad)
  ]

--------------------------------------------------------------------------------

data Logger m = Logger (MVar m LogCommand) (MVar m [String])

data LogCommand = Message String | Stop

-- | Create a new logger with no internal log.
initLogger :: MonadConc m => m (Logger m)
initLogger = do
  cmd <- newEmptyMVar
  logg <- newMVar []
  let l = Logger cmd logg
  void . fork $ logger l
  return l

logger :: MonadConc m => Logger m -> m ()
logger (Logger cmd logg) = loop where
  loop = do
    command <- takeMVar cmd
    case command of
      Message str -> do
        strs <- takeMVar logg
        putMVar logg (strs ++ [str])
        loop
      Stop -> return ()

-- | Add a string to the log.
logMessage :: MonadConc m => Logger m -> String -> m ()
logMessage (Logger cmd _) str = putMVar cmd $ Message str

-- | Stop the logger and return the contents of the log.
logStop :: MonadConc m => Logger m -> m [String]
logStop (Logger cmd logg) = do
  putMVar cmd Stop
  readMVar logg

-- | Race condition! Can you see where?
raceyLogger :: MonadConc m => m [String]
raceyLogger = do
  l <- initLogger
  logMessage l "Hello"
  logMessage l "World"
  logMessage l "Foo"
  logMessage l "Bar"
  logMessage l "Baz"
  logStop l

-- | Test that the result is always in the set of allowed values, and
-- doesn't deadlock.
validResult :: Predicate [String]
validResult = alwaysTrue check where
  check (Right strs) = strs `elem` [ ["Hello", "World", "Foo", "Bar", "Baz"]
                                   , ["Hello", "World", "Foo", "Bar"]
                                   ]
  check _ = False

-- | Test that the "proper" result occurs at least once.
isGood :: Predicate [String]
isGood = somewhereTrue check where
  check (Right a) = length a == 5
  check _ = False

-- | Test that the erroneous result occurs at least once.
isBad :: Predicate [String]
isBad = somewhereTrue check where
  check (Right a) = length a == 4
  check _ = False
