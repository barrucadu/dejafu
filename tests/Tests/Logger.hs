-- Modification (to introduce bug) of an example in Parallel and
-- Concurrent Programming in Haskell, chapter 7.
module Tests.Logger
  ( badLogger
  , validResult, isGood, isBad
  ) where

import Control.Concurrent.CVar
import Control.Monad (void)
import Control.Monad.Conc.Class
import Test.DejaFu

data Logger m = Logger (CVar m LogCommand) (CVar m [String])

data LogCommand = Message String | Stop

-- | Create a new logger with no internal log.
initLogger :: MonadConc m => m (Logger m)
initLogger = do
  cmd <- newEmptyCVar
  logg <- newCVar []
  let l = Logger cmd logg
  void . fork $ logger l
  return l

logger :: MonadConc m => Logger m -> m ()
logger (Logger cmd logg) = loop where
  loop = do
    command <- takeCVar cmd
    case command of
      Message str -> do
        strs <- takeCVar logg
        putCVar logg (strs ++ [str])
        loop
      Stop -> return ()

-- | Add a string to the log.
logMessage :: MonadConc m => Logger m -> String -> m ()
logMessage (Logger cmd _) str = putCVar cmd $ Message str

-- | Stop the logger and return the contents of the log.
logStop :: MonadConc m => Logger m -> m [String]
logStop (Logger cmd logg) = do
  putCVar cmd Stop
  readCVar logg

-- | Race condition! Can you see where?
badLogger :: MonadConc m => m [String]
badLogger = do
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
