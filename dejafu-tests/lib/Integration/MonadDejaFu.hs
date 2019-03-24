module Integration.MonadDejaFu where

import qualified Control.Concurrent.Classy as C

import           Control.Monad.Catch.Pure  (runCatchT)
import           Control.Monad.ST          (runST)
import           Test.DejaFu.Conc          (Condition(..), Program,
                                            roundRobinSched, runConcurrent)
import           Test.DejaFu.Settings      (defaultMemType)
import           Test.DejaFu.Types         (MonadDejaFu)
import qualified Test.Tasty.HUnit          as TH

import           Common

tests :: [TestTree]
tests =
  [ testGroup "IO" ioTests
  , testGroup "ST" stTests
  ]

--------------------------------------------------------------------------------

ioTests :: [TestTree]
ioTests = toTestList
  [ TH.testCase "Supports bound threads" $
      let res = single C.supportsBoundThreads
      in TH.assertEqual "" (Right True) =<< res

  , TH.testCase "Main thread is bound" $
      let res = single C.isCurrentThreadBound
      in TH.assertEqual "" (Right True) =<< res

  , TH.testCase "Can fork bound threads" $
      let res = single $ do
            _ <- C.forkOS (pure ())
            pure True
      in TH.assertEqual "" (Right True) =<< res
  ]

--------------------------------------------------------------------------------

stTests :: [TestTree]
stTests = toTestList
  [ TH.testCase "Doesn't support bound threads" $
      let res = runST $ runCatchT $ single C.supportsBoundThreads
      in TH.assertEqual "" (Right (Right False)) res

  , TH.testCase "Main thread isn't bound" $
      let res = runST $ runCatchT $ single C.isCurrentThreadBound
      in TH.assertEqual "" (Right (Right False)) res

  , TH.testCase "Can't fork bound threads" $
      let res = runST $ runCatchT $ single $ do
            _ <- C.forkOS (pure ())
            pure True
      in case res of
           Right (Left (UncaughtException _)) -> pure ()
           _ -> TH.assertFailure ("expected: Right (Left (UncaughtException _))\n but got: " ++ show res)
  ]

--------------------------------------------------------------------------------

single :: MonadDejaFu n => Program pty n a -> n (Either Condition a)
single program =
  let fst3 (a, _, _) = a
  in fst3 <$> runConcurrent roundRobinSched defaultMemType () program
