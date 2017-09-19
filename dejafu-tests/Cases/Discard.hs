module Cases.Discard where

import Control.Concurrent.Classy
import Test.DejaFu (gives')
import Test.HUnit.DejaFu (Discard(..), defaultMemType, defaultWay, testDejafuDiscard)

import Common

tests :: [Test]
tests = toTestList
  [ check "All results are kept when none are discarded" [1, 2, 3] $
      const Nothing
  , check "No results are kept when all are discarded" [] $
      const (Just DiscardResultAndTrace)
  , check "Results failing the test are not present" [1, 2] $
      \x -> if x == Right 3 then Just DiscardResultAndTrace else Nothing
  ]
  where
    check name xs f = testDejafuDiscard f defaultWay defaultMemType nondet name (gives' xs)

nondet :: MonadConc m => m Int
nondet = do
  mvar <- newEmptyMVar
  _ <- fork $ putMVar mvar 1
  _ <- fork $ putMVar mvar 2
  _ <- fork $ putMVar mvar 3
  readMVar mvar
