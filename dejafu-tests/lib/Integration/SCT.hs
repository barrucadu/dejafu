module Integration.SCT where

import           Control.Concurrent.Classy
import           Control.Monad             (void)
import qualified Data.Set                  as S
import           Test.DejaFu.SCT
import           Test.DejaFu.Types         (Failure(..))
import           Test.Tasty.HUnit

import           Common

tests :: [TestTree]
tests =
  toTestList
    [ testCase "Proper results from resultsSet" $ do
        tested <- resultsSet defaultWay defaultMemType testAction
        results @=? tested
    , testCase "Proper results from resultsSet'" $ do
        tested <- resultsSet' defaultWay defaultMemType testAction
        results @=? tested
    , testCase "Proper results from resultsSetDiscard" $ do
        tested <-
          resultsSetDiscard discarder defaultWay defaultMemType testAction
        resultsWithDiscard @=? tested
    , testCase "Proper results from resultsSetDiscard'" $ do
        tested <-
          resultsSetDiscard' discarder defaultWay defaultMemType testAction
        resultsWithDiscard @=? tested
    ]
  where
    results = S.fromList $ map Right [1, 2] ++ [Left Deadlock]
    resultsWithDiscard = S.fromList [Right 2, Left Deadlock]
    discarder (Right 1) = Just DiscardResultAndTrace
    discarder (Right 2) = Just DiscardTrace
    discarder _ = Nothing
    testAction = do
      mvar <- newEmptyMVarInt
      _ <- fork $ putMVar mvar 1
      _ <- fork $ putMVar mvar 2
      _ <- fork $ mapM_ (\_ -> void $ takeMVar mvar) [1 :: Int, 2]
      readMVar mvar
