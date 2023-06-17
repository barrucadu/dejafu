{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
The tasty-hedgehog package:
http://hackage.haskell.org/package/tasty-hedgehog

This is the verbatim contents of tasty-hedgehog, as of version
0.1.0.2.  The original code is available under the 3-clause BSD
license, which is reproduced below.

- - - - -

Copyright (c) 2017, Commonwealth Scientific and Industrial Research Organisation
(CSIRO) ABN 41 687 119 230.

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of QFPL nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

-- | This package lets you test Hedgehog properties with tasty.
--
-- Typical usage would look like this:
--
-- @
-- testGroup "tasty-hedgehog tests" [
--    testProperty "reverse involutive" prop_reverse_involutive
--  , testProperty "sort idempotent"    prop_sort_idempotent
--  ]
-- @
--
module Test.Tasty.Hedgehog (
    testProperty
  -- * Options you can pass in via tasty
  , HedgehogReplay(..)
  , HedgehogShowReplay(..)
  , HedgehogVerbose(..)
  , HedgehogTestLimit(..)
  , HedgehogDiscardLimit(..)
  , HedgehogShrinkLimit(..)
  , HedgehogShrinkRetries(..)
  ) where

import           Control.Monad.IO.Class     (MonadIO)
import           Data.Typeable

import           Test.Tasty.Options
import qualified Test.Tasty.Providers       as T

import           Hedgehog
import           Hedgehog.Internal.Config   (UseColor(EnableColor))
import           Hedgehog.Internal.Property
import           Hedgehog.Internal.Report
import           Hedgehog.Internal.Runner   as H
import           Hedgehog.Internal.Seed     as Seed

data HP = HP T.TestName Property
  deriving (Typeable)

-- | Create a 'Test' from a Hedgehog property
testProperty :: T.TestName -> Property -> T.TestTree
testProperty name prop = T.singleTest name (HP name prop)

-- | The replay token to use for replaying a previous test run
newtype HedgehogReplay = HedgehogReplay (Maybe (Size, Seed))
  deriving (Typeable)

instance IsOption HedgehogReplay where
  defaultValue = HedgehogReplay Nothing
  parseValue v = HedgehogReplay . Just <$> replay
    -- Reads a replay token in the form "{size} {seed}"
    where replay = (,) <$> safeRead (unwords size) <*> safeRead (unwords seed)
          (size, seed) = splitAt 2 $ words v
  optionName = pure "hedgehog-replay"
  optionHelp = pure "Replay token to use for replaying a previous test run"

-- | If a test case fails, show a replay token for replaying tests
newtype HedgehogShowReplay = HedgehogShowReplay Bool
  deriving (Typeable)

instance IsOption HedgehogShowReplay where
  defaultValue = HedgehogShowReplay True
  parseValue = fmap HedgehogShowReplay . safeRead
  optionName = pure "hedgehog-show-replay"
  optionHelp = pure "Show a replay token for replaying tests"

-- | Show the generated Hedgehog test cases
newtype HedgehogVerbose = HedgehogVerbose Bool
  deriving (Typeable)

instance IsOption HedgehogVerbose where
  defaultValue = HedgehogVerbose False
  parseValue = fmap HedgehogVerbose . safeRead
  optionName = pure "hedgehog-verbose"
  optionHelp = pure "Show the generated Hedgehog test cases"
  optionCLParser = flagCLParser Nothing (HedgehogVerbose True)

-- | The number of successful test cases required before Hedgehog will pass a test
newtype HedgehogTestLimit = HedgehogTestLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Typeable)

instance IsOption HedgehogTestLimit where
  defaultValue = 100
  parseValue = fmap HedgehogTestLimit . safeRead
  optionName = pure "hedgehog-tests"
  optionHelp = pure "Number of successful test cases required before Hedgehog will pass a test"

-- | The number of discarded cases allowed before Hedgehog will fail a test
newtype HedgehogDiscardLimit = HedgehogDiscardLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Typeable)

instance IsOption HedgehogDiscardLimit where
  defaultValue = 100
  parseValue = fmap HedgehogDiscardLimit . safeRead
  optionName = pure "hedgehog-discards"
  optionHelp = pure "Number of discarded cases allowed before Hedgehog will fail a test"

-- | The number of shrinks allowed before Hedgehog will fail a test
newtype HedgehogShrinkLimit = HedgehogShrinkLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Typeable)

instance IsOption HedgehogShrinkLimit where
  defaultValue = 100
  parseValue = fmap HedgehogShrinkLimit . safeRead
  optionName = pure "hedgehog-shrinks"
  optionHelp = pure "Number of shrinks allowed before Hedgehog will fail a test"

-- | The number of times to re-run a test during shrinking
newtype HedgehogShrinkRetries = HedgehogShrinkRetries Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Typeable)

instance IsOption HedgehogShrinkRetries where
  defaultValue = 10
  parseValue = fmap HedgehogShrinkRetries . safeRead
  optionName = pure "hedgehog-retries"
  optionHelp = pure "Number of times to re-run a test during shrinking"

getReport :: Report a -> (TestCount, a)
#if MIN_VERSION_hedgehog(1,0,0)
getReport r = (reportTests r, reportStatus r)
#else
getReport (Report testCount _ status) = (testCount, status)
#endif

reportToProgress :: Int
                 -> Int
                 -> Int
                 -> Report Progress
                 -> T.Progress
reportToProgress testLimit _ shrinkLimit report =
  let
    (testsDone, status) = getReport report
    ratio x y = 1.0 * fromIntegral x / fromIntegral y
  in
    -- TODO add details for tests run / discarded / shrunk
    case status of
      Running ->
        T.Progress "Running" (ratio testsDone testLimit)
      Shrinking fr ->
        T.Progress "Shrinking" (ratio (failureShrinks fr) shrinkLimit)

renderResult' :: MonadIO m => Maybe PropertyName -> Report Result -> m String
#if MIN_VERSION_hedgehog(1,0,2)
renderResult' = renderResult EnableColor
#else
renderResult' = renderResult (Just EnableColor)
#endif

reportOutput :: Bool
             -> Bool
             -> String
             -> Report Result
             -> IO String
reportOutput _ _ name report = do
  let (_, status) = getReport report
  s <- renderResult' (Just (PropertyName name)) report
  pure $ case status of
    Failed _ -> s
    GaveUp -> "Gave up"
    OK -> "OK"

propertyConfig' :: TestLimit -> DiscardLimit -> ShrinkLimit -> ShrinkRetries -> PropertyConfig
#if MIN_VERSION_hedgehog(1,2,0)
propertyConfig' testLimit discardLimit shrinkLimit shrinkRetries = PropertyConfig
  { propertyDiscardLimit        = discardLimit
  , propertyShrinkLimit         = shrinkLimit
  , propertyShrinkRetries       = shrinkRetries
  , propertyTerminationCriteria = NoConfidenceTermination testLimit
  , propertySkip                = Nothing
  }
#elif MIN_VERSION_hedgehog(1,0,2)
propertyConfig' testLimit discardLimit shrinkLimit shrinkRetries = PropertyConfig
  { propertyDiscardLimit        = discardLimit
  , propertyShrinkLimit         = shrinkLimit
  , propertyShrinkRetries       = shrinkRetries
  , propertyTerminationCriteria = NoConfidenceTermination testLimit
  }
#else
propertyConfig' testLimit discardLimit shrinkLimit shrinkRetries = PropertyConfig
  { propertyDiscardLimit  = discardLimit
  , propertyShrinkLimit   = shrinkLimit
  , propertyShrinkRetries = shrinkRetries
  , propertyTestLimit     = testLimit
  }
#endif

instance T.IsTest HP where
  testOptions =
    pure [ Option (Proxy :: Proxy HedgehogReplay)
           , Option (Proxy :: Proxy HedgehogShowReplay)
           , Option (Proxy :: Proxy HedgehogVerbose)
           , Option (Proxy :: Proxy HedgehogTestLimit)
           , Option (Proxy :: Proxy HedgehogDiscardLimit)
           , Option (Proxy :: Proxy HedgehogShrinkLimit)
           , Option (Proxy :: Proxy HedgehogShrinkRetries)
           ]

  run opts (HP name (Property _ pTest)) yieldProgress = do
    let
      HedgehogReplay         replay = lookupOption opts
      HedgehogShowReplay showReplay = lookupOption opts
      HedgehogVerbose       verbose = lookupOption opts
      HedgehogTestLimit       tests = lookupOption opts
      HedgehogDiscardLimit discards = lookupOption opts
      HedgehogShrinkLimit   shrinks = lookupOption opts
      HedgehogShrinkRetries retries = lookupOption opts
      config = propertyConfig'
          (TestLimit tests)
          (DiscardLimit discards)
          (ShrinkLimit shrinks)
          (ShrinkRetries retries)

    randSeed <- Seed.random
    let
      size = maybe 0 fst replay
      seed = maybe randSeed snd replay

    report <- checkReport config size seed pTest (yieldProgress . reportToProgress tests discards shrinks)

    let
      resultFn = if reportStatus report == OK
                 then T.testPassed
                 else T.testFailed

    out <- reportOutput verbose showReplay name report
    pure $ resultFn out
