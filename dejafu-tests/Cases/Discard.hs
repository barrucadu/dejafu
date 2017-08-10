module Cases.Discard where

import Control.Concurrent.Classy
import Test.DejaFu (gives')
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (test)
import Test.HUnit.DejaFu (Discard(..), defaultMemType, defaultWay, testDejafuDiscard)

tests :: [Test]
tests = hUnitTestToTests $ test
  [ check "all results"  [1, 2, 3] (const Nothing)
  , check "no results"   []        (const $ Just DiscardResultAndTrace)
  , check "some results" [1, 2]    (\x -> if x == Right 3 then Just DiscardResultAndTrace else Nothing)
  ]
  where
    check name xs f = testDejafuDiscard f defaultWay defaultMemType nondet name (gives' xs)

nondet :: MonadConc m => m Int
nondet = do
  mvar <- newEmptyMVar
  fork $ putMVar mvar 1
  fork $ putMVar mvar 2
  fork $ putMVar mvar 3
  readMVar mvar
