module Integration.Names where

import           Control.Concurrent.Classy hiding (check)
import           Data.Maybe                (mapMaybe)
import           Test.DejaFu.Conc          (ConcIO)
import           Test.DejaFu.Internal      (crefOf, mvarOf, simplifyAction,
                                            tidsOf, tvarsOf)
import           Test.DejaFu.SCT           (runSCT)
import           Test.DejaFu.Types
import           Test.Tasty.HUnit

import           Common

tests :: [TestTree]
tests =
  toTestList
    [ testCase "MVar names" testMVarNames
    , testCase "CRef names" testCRefNames
    , testCase "TVar names" testTVarNames
    , testCase "Thread names" testThreadNames
    ]

check ::
     String
  -> ([ThreadAction] -> Bool)
  -> ConcIO a
  -> Assertion
check msg validActions testAction = do
  outcomes <- runSCT defaultWay defaultMemType testAction
  let extractActions = map $ \(_, _, action) -> action
      actions = [extractActions trace | (_, trace) <- outcomes]
  assertBool msg $ any validActions actions

testMVarNames :: Assertion
testMVarNames =
  check "All traces should use only required MVar names" checkMVars $ do
    mvar1 <- newEmptyMVarN mvarName1
    mvar2 <- newEmptyMVarN mvarName2
    _ <- takeMVar mvar1
    _ <- fork $ putMVar mvar1 (1 :: Int)
    _ <- fork $ putMVar mvar2 (2 :: Int)
    _ <- fork $ putMVar mvar1 3
    (,) <$> readMVar mvar1 <*> readMVar mvar2
  where
    mvarName1 = "first-mvar"
    mvarName2 = "second-mvar"
    mvarName (MVarId (Id (Just n) _)) = Just n
    mvarName _ = Nothing
    mvar (NewMVar mvid) = Just mvid
    mvar a = mvarOf (simplifyAction a)
    checkMVars =
      let validMVid = maybe False (`elem` [mvarName1, mvarName2]) . mvarName
      in all validMVid . mapMaybe mvar

testCRefNames :: Assertion
testCRefNames =
  check "All traces should use only required CRef names" checkCRefs $ do
    x <- newCRefN crefName1 (0::Int)
    y <- newCRefN crefName2 (0::Int)
    _ <- fork $ modifyCRefCAS x (const (1, ()))
    _ <- fork $ writeCRef y 2
    (,) <$> readCRef x <*> readCRef y
  where
    crefName1 = "cref-one"
    crefName2 = "cref-two"
    crefName (CRefId (Id (Just n) _)) = Just n
    crefName _ = Nothing
    cref (NewCRef ref) = Just ref
    cref a = crefOf (simplifyAction a)
    checkCRefs =
      let validCRef = maybe False (`elem` [crefName1, crefName2]) . crefName
      in all validCRef . mapMaybe cref

testTVarNames :: Assertion
testTVarNames =
  check "All traces should use only required TVar names" checkTVars $ do
    v1 <- atomically $ newTVarN tvarName1 (0::Int)
    v2 <- atomically $ newTVarN  tvarName2 (0::Int)
    _ <-
      fork . atomically $ do
        writeTVar v1 1
        modifyTVar v2 (+ 100)
    _ <-
      fork . atomically $ do
        modifyTVar v1 (* 100)
        writeTVar v2 42
    pure ()
  where
    tvarName1 = "tvar-one"
    tvarName2 = "tvar-two"
    tvarName (TVarId (Id (Just n) _)) = Just n
    tvarName _ = Nothing
    checkTVars =
      let validTVar =
            maybe False (`elem` [tvarName1, tvarName2]) . tvarName
      in all (all validTVar) . map tvarsOf

testThreadNames :: Assertion
testThreadNames =
  check "All traces should use only required thread names" checkThreads $ do
      x <- newEmptyMVar
      tid <- forkN threadName2 $ putMVar x ()
      _ <- forkN threadName1 $ readMVar x
      _ <- forkN threadName3 $ pure ()
      killThread tid
  where
    threadName1 = "thread-one"
    threadName2 = "thread-two"
    threadName3 = "thread-three"
    threadName (ThreadId (Id (Just n) _)) = Just n
    threadName _ = Nothing
    checkThreads =
      let validTid =
            maybe False (`elem` [threadName1, threadName2, threadName3]) .
            threadName
      in all (all validTid) . map tidsOf
