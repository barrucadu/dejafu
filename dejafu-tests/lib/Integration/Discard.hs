module Integration.Discard where

import           Control.Concurrent.Classy hiding (check)
import           Test.DejaFu               (gives')

import           Common

tests :: [TestTree]
tests = toTestList
  [ check "All results are kept when none are discarded" [1, 2, 3] $
      const Nothing
  , check "No results are kept when all are discarded" [] $
      const (Just DiscardResultAndTrace)
  , check "Results failing the test are not present" [1, 2] $
      \x -> if x == Right 3 then Just DiscardResultAndTrace else Nothing
  ]
  where
    check name xs f = testDejafuDiscard f defaultWay defaultMemType name (gives' xs) $ do
      mvar <- newEmptyMVarInt
      _ <- fork $ putMVar mvar 1
      _ <- fork $ putMVar mvar 2
      _ <- fork $ putMVar mvar 3
      readMVar mvar
