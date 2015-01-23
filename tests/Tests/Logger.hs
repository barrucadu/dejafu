-- Modification (to introduce bug) of an example in Parallel and
-- Concurrent Programming in Haskell, chapter 7.
module Tests.Logger
  ( Logger
  , initLogger
  , logMessage
  , logStop
  , bad
  , testLA, testLP, testLE
  ) where

import Control.Monad.Conc.Class
import Control.Monad.Conc.CVar
import Control.Monad.Conc.SCT.Tests

data Logger cvar = Logger (cvar LogCommand) (cvar [String])

data LogCommand = Message String | Stop

-- | Create a new logger with no internal log.
initLogger :: ConcCVar cvar m => m (Logger cvar)
initLogger = do
  cmd <- newEmptyCVar
  log <- newCVar []
  let l = Logger cmd log
  fork $ logger l
  return l

logger :: ConcCVar cvar m => Logger cvar -> m ()
logger (Logger cmd log) = loop where
  loop = do
    command <- takeCVar cmd
    case command of
      Message str -> do
        strs <- takeCVar log
        putCVar log (strs ++ [str])
        loop
      Stop -> return ()

-- | Add a string to the log.
logMessage :: ConcCVar cvar m => Logger cvar -> String -> m ()
logMessage (Logger cmd _) str = putCVar cmd $ Message str

-- | Stop the logger and return the contents of the log.
logStop :: ConcCVar cvar m => Logger cvar -> m [String]
logStop (Logger cmd log) = do
  putCVar cmd Stop
  readCVar log

-- | Race condition! Can you see where?
bad :: ConcCVar cvar m => m [String]
bad = do
  l <- initLogger
  logMessage l "Hello"
  logMessage l "World"
  logMessage l "Foo"
  logMessage l "Bar"
  logMessage l "Baz"
  logStop l

-- | Test that the result is always in the set of allowed values, and
-- doesn't deadlock.
testLA :: Result [String]
testLA = runTest (alwaysTrue listContents) bad where
  listContents (Just strs) = strs `elem` [ ["Hello", "World", "Foo", "Bar", "Baz"]
                                         , ["Hello", "World", "Foo", "Bar"]
                                         ]
  listContents Nothing = False

-- | Test that the "proper" result occurs at least once.
testLP :: Result [String]
testLP = runTest (somewhereTrue loggedAll) bad where
  loggedAll (Just a) = length a == 5
  loggedAll Nothing  = False

-- | Test that the erroneous result occurs at least once.
testLE :: Result [String]
testLE = runTest (somewhereTrue loggedAlmostAll) bad where
  loggedAlmostAll (Just a) = length a == 4
  loggedAlmostAll Nothing  = False
