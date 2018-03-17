module Integration.SCT where

import           Control.Concurrent.Classy hiding (check)
import           Control.Monad             (void)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Foldable             (for_)
import qualified Data.IORef                as IORef
import qualified Data.Set                  as S
import           System.Random             (mkStdGen)
import           Test.DejaFu               (gives')
import           Test.DejaFu.SCT
import           Test.DejaFu.Types         (Failure(..))
import           Test.Tasty.HUnit

import           Common

tests :: [TestTree]
tests =
  [ testGroup "Discard" discardTests
  , testGroup "EarlyExit" earlyExitTests
  , testGroup "Results" resultsSetTests
  ]

-------------------------------------------------------------------------------

discardTests :: [TestTree]
discardTests = toTestList
    [ check "All results are kept when none are discarded" [1, 2, 3] $
        const Nothing
    , check "No results are kept when all are discarded" [] $
        const (Just DiscardResultAndTrace)
    , check "Results failing the test are not present" [1, 2] $
        \x -> if x == Right 3 then Just DiscardResultAndTrace else Nothing
    , testCase "No traces kept when they get discared" $ testDiscardTrace testAction
    ]
  where
    check name xs f = testDejafuWithSettings (set ldiscard (Just f) defaultSettings) name (gives' xs) testAction
    testAction = do
      mvar <- newEmptyMVarInt
      _ <- fork $ putMVar mvar 1
      _ <- fork $ putMVar mvar 2
      _ <- fork $ putMVar mvar 3
      readMVar mvar

    discarder (Right 2) = Just DiscardTrace
    discarder (Right 3) = Just DiscardResultAndTrace
    discarder  _ = Nothing

    testDiscardTrace action = do
      results <- runSCTWithSettings (set ldiscard (Just discarder) defaultSettings) action
      for_ results $ \(efa, trace) -> case discarder efa of
        Just DiscardResultAndTrace -> assertFailure "expected result to be discarded"
        Just DiscardTrace
          | null trace -> pure ()
           | otherwise -> assertFailure "expected trace to be discarded"
        Nothing -> pure ()

-------------------------------------------------------------------------------

earlyExitTests :: [TestTree]
earlyExitTests = toTestList
    [ eeTest "Without discarding" [1,2,3,4,5] Nothing
    , eeTest "Discarding some result" [1,2,4,5] $ Just (\efa -> if efa == Right 3 then Just DiscardResultAndTrace else Nothing)
    , eeTest "Discarding the stop condition" [1,2,3,4] $ Just (\efa -> if efa == Right 5 then Just DiscardResultAndTrace else Nothing)
    ]
  where
    eeTest name expected d = testCase name $ do
      -- abuse IO to get a different result form every execution
      r <- liftIO (IORef.newIORef (0::Int))
      actual <- resultsSetWithSettings (eeSettings d) $ do
        liftIO (IORef.modifyIORef r (+1))
        liftIO (IORef.readIORef r)
      S.fromList (map Right expected) @=? actual

    eeSettings d =
      set ldiscard d $
      set learlyExit (Just (==Right 5)) $
      fromWayAndMemType (randomly (mkStdGen 0) 150) defaultMemType

-------------------------------------------------------------------------------

resultsSetTests :: [TestTree]
resultsSetTests = toTestList
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
