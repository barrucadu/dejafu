module Integration.Discard where

import           Control.Concurrent.Classy hiding (check)
import           Data.Foldable             (for_)
import           Test.DejaFu               (gives')
import           Test.DejaFu.Conc          (ConcIO)
import           Test.DejaFu.SCT
import           Test.DejaFu.Types         (Failure)
import           Test.Tasty.HUnit

import           Common

tests :: [TestTree]
tests = toTestList
  [ check "All results are kept when none are discarded" [1, 2, 3] $
      const Nothing
  , check "No results are kept when all are discarded" [] $
      const (Just DiscardResultAndTrace)
  , check "Results failing the test are not present" [1, 2] $
      \x -> if x == Right 3 then Just DiscardResultAndTrace else Nothing
  , testCase "No traces kept when they get discared" $ testDiscardTrace discarder testAction
  ]
  where
    check name xs f = testDejafuDiscard f defaultWay defaultMemType name (gives' xs) testAction
    testAction = do
      mvar <- newEmptyMVarInt
      _ <- fork $ putMVar mvar 1
      _ <- fork $ putMVar mvar 2
      _ <- fork $ putMVar mvar 3
      readMVar mvar
    discarder (Right 2) = Just DiscardTrace
    discarder (Right 3) = Just DiscardResultAndTrace
    discarder  _ = Nothing

testDiscardTrace :: (Either Failure a -> Maybe Discard) -> ConcIO a -> Assertion
testDiscardTrace discarder action = do
  results <- runSCTDiscard discarder defaultWay defaultMemType action
  for_ results $ \(efa, trace) -> case discarder efa of
    Just DiscardResultAndTrace -> assertFailure "expected result to be discarded"
    Just DiscardTrace
      | null trace -> pure ()
      | otherwise -> assertFailure "expected trace to be discarded"
    Nothing -> pure ()
