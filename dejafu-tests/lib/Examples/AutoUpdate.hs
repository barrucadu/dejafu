{-# LANGUAGE ScopedTypeVariables #-}

{-
The auto-update package:
https://hackage.haskell.org/package/auto-update

Users found a possible deadlock and livelock:
https://www.reddit.com/r/haskell/comments/2i5d7m/updating_autoupdate/

This is the code from Control.AutoUpdate modified to use the
@MonadConc@ abstraction, with tests added to verify that the issues
identified are caught.. The original code is available under the MIT
license, which is reproduced below.

- - - - -

Copyright (c) 2014 Michael Snoyman

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Examples.AutoUpdate where

import           Control.Exception        (SomeException)
import           Control.Monad
import           Control.Monad.Conc.Class

-- test imports
import           Common
import           Test.DejaFu              (Condition(..), basic, gives)

tests :: [TestTree]
tests = toTestList
  [ T "deadlocks"        (basic deadlocks)        (gives [Left Deadlock, Right ()])
  , T "nondeterministic" (basic nondeterministic) (gives [Left Deadlock, Right 0, Right 1])
  ]

-- This exhibits a deadlock with no preemptions.
deadlocks :: MonadConc m => m ()
deadlocks = join (mkAutoUpdate defaultUpdateSettings)

-- This exhibits nondeterminism with three preemptions.  However, as
-- the program explicitly yields, the bounds don't need changing.
nondeterministic :: forall m. MonadConc m => m Int
nondeterministic = do
  var <- newIORef 0
  let settings = (defaultUpdateSettings :: UpdateSettings m ())
        { updateAction = atomicModifyIORef var (\x -> (x+1, x)) }
  auto <- mkAutoUpdate settings
  void auto
  auto

-------------------------------------------------------------------------------

data UpdateSettings m a = UpdateSettings
    { updateFreq           :: Int
    , updateSpawnThreshold :: Int
    , updateAction         :: m a
    }

defaultUpdateSettings :: MonadConc m => UpdateSettings m ()
defaultUpdateSettings = UpdateSettings
    { updateFreq           = 1000000
    , updateSpawnThreshold = 3
    , updateAction         = pure ()
    }

mkAutoUpdate :: MonadConc m => UpdateSettings m a -> m (m a)
mkAutoUpdate us = do
    currRef      <- newIORef Nothing
    needsRunning <- newEmptyMVar
    lastValue    <- newEmptyMVar

    void $ fork $ forever $ do
        takeMVar needsRunning

        a <- catchSome $ updateAction us

        writeIORef currRef $ Just a
        void $ tryTakeMVar lastValue
        putMVar lastValue a

        threadDelay $ updateFreq us

        writeIORef currRef Nothing
        void $ takeMVar lastValue

    pure $ do
        mval <- readIORef currRef
        case mval of
            Just val -> pure val
            Nothing -> do
                void $ tryPutMVar needsRunning ()
                readMVar lastValue

catchSome :: MonadConc m => m a -> m a
catchSome act = catch act $
  \e -> throw (e :: SomeException)
