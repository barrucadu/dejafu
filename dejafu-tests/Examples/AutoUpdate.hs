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

import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.Conc.Class

-- test imports
import System.Random (StdGen, mkStdGen)
import Test.DejaFu (Bounds(..), Failure(..), MemType(..), Way(..), defaultBounds, defaultWay, gives)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (test)
import Test.HUnit.DejaFu

tests :: [Test]
tests =
  [ testGroup "Systematic" . hUnitTestToTests $ test
    [ testDejafuWay defaultWay
                    SequentialConsistency
                    deadlocks
                    "deadlocks"
                    (gives [Left Deadlock, Right ()])

    , testDejafuWay (Systematically defaultBounds { boundPreemp = Just 3 })
                    SequentialConsistency
                    nondeterministic
                    "nondeterministic (systematic)"
                    (gives [Left Deadlock, Right 0, Right 1])
    ]
  , testGroup "Random" . hUnitTestToTests $ test
    [ testDejafuWay (Randomly (mkStdGen 0) 100)
                    SequentialConsistency
                    deadlocks
                    "deadlocks (random)"
                    (gives [Left Deadlock, Right ()])

    , testDejafuWay (Randomly (mkStdGen 0) 100)
                    SequentialConsistency
                    nondeterministic
                    "nondeterministic (random)"
                    (gives [Left Deadlock, Right 0, Right 1])
    ]
  ]

-- This exhibits a deadlock with no preemptions.
deadlocks :: MonadConc m => m ()
deadlocks = join (mkAutoUpdate defaultUpdateSettings)

-- This exhibits nondeterminism with three preemptions.
nondeterministic :: forall m. MonadConc m => m Int
nondeterministic = do
  var <- newCRef 0
  let settings = (defaultUpdateSettings :: UpdateSettings m ())
        { updateAction = atomicModifyCRef var (\x -> (x+1, x)) }
  auto <- mkAutoUpdate settings
  auto
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
    , updateAction         = return ()
    }

mkAutoUpdate :: MonadConc m => UpdateSettings m a -> m (m a)
mkAutoUpdate us = do
    currRef      <- newCRef Nothing
    needsRunning <- newEmptyMVar
    lastValue    <- newEmptyMVar

    void $ fork $ forever $ do
        takeMVar needsRunning

        a <- catchSome $ updateAction us

        writeCRef currRef $ Just a
        void $ tryTakeMVar lastValue
        putMVar lastValue a

        threadDelay $ updateFreq us

        writeCRef currRef Nothing
        void $ takeMVar lastValue

    return $ do
        mval <- readCRef currRef
        case mval of
            Just val -> return val
            Nothing -> do
                void $ tryPutMVar needsRunning ()
                readMVar lastValue

catchSome :: MonadConc m => m a -> m a
catchSome act = catch act $
  \e -> throw (e :: SomeException)
