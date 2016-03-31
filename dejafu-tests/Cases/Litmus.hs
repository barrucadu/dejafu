module Cases.Litmus where

import Test.DejaFu (MemType(..), defaultBounds, gives')
import Test.DejaFu.Deterministic (ConcST)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (test)
import Test.HUnit.DejaFu (testDejafu')

import Control.Monad.Conc.Class

tests :: [Test]
tests =
  [ litmusTest "intelWP21" intelWP21
      [(0,0),(0,1),(1,1)]
      [(0,0),(0,1),(1,1)]
      [(0,0),(0,1),(1,0),(1,1)]

  , litmusTest "intelWP22" intelWP22
      [(0,0),(0,1),(1,0)]
      [(0,0),(0,1),(1,0)]
      [(0,0),(0,1),(1,0)]

  , litmusTest "intelWP23" intelWP23
      [(0,1),(1,0),(1,1)]
      [(0,0),(0,1),(1,0),(1,1)]
      [(0,0),(0,1),(1,0),(1,1)]

  , litmusTest "intelWP24" intelWP24
      [(1,1)]
      [(1,1)]
      [(1,1)]

  , litmusTest "intelWP25" intelWP25
      [((1,0),(1,1)),((1,1),(1,0)),((1,1),(1,1))]
      [((1,0),(1,0)),((1,0),(1,1)),((1,1),(1,0)),((1,1),(1,1))]
      [((1,0),(1,0)),((1,0),(1,1)),((1,1),(1,0)),((1,1),(1,1))]

  , litmusTest "intelWP26" intelWP26
      [(0,0,0),(0,0,1),(1,0,1)]
      [(0,0,0),(0,0,1),(1,0,0),(1,0,1)]
      [(0,0,0),(0,0,1),(1,0,0),(1,0,1)]

  , litmusTest "intelWP27" intelWP27
      [((0,0),(0,0)),((0,0),(1,1)),((0,0),(2,2)),((1,1),(1,1)),((1,1),(1,2)),((1,1),(2,1)),((1,1),(2,2)),((1,2),(1,1)),((1,2),(2,2)),((2,1),(1,1)),((2,1),(2,2)),((2,2),(1,1)),((2,2),(2,1)),((2,2),(2,2))]
      [((0,0),(0,0)),((0,0),(1,1)),((0,0),(2,2)),((0,1),(1,1)),((0,1),(2,2)),((0,2),(1,1)),((0,2),(2,2)),((1,1),(1,1)),((1,1),(1,2)),((1,1),(2,1)),((1,1),(2,2)),((1,2),(1,1)),((1,2),(1,2)),((1,2),(2,2)),((2,1),(1,1)),((2,1),(2,2)),((2,2),(1,1)),((2,2),(1,2)),((2,2),(2,1)),((2,2),(2,2))]
      [((0,0),(0,0)),((0,0),(1,1)),((0,0),(2,2)),((0,1),(1,1)),((0,1),(2,2)),((0,2),(1,1)),((0,2),(2,2)),((1,1),(1,1)),((1,1),(1,2)),((1,1),(2,1)),((1,1),(2,2)),((1,2),(1,1)),((1,2),(1,2)),((1,2),(2,2)),((2,1),(1,1)),((2,1),(2,2)),((2,2),(1,1)),((2,2),(1,2)),((2,2),(2,1)),((2,2),(2,2))]

  , litmusTest "intelWP28" intelWP28
      [((0,0),(0,0)),((0,0),(0,1)),((0,0),(1,0)),((0,0),(1,1)),((0,1),(1,0)),((0,1),(1,1)),((1,0),(0,1)),((1,0),(1,1)),((1,1),(1,1))]
      [((0,0),(0,0)),((0,0),(0,1)),((0,0),(1,0)),((0,0),(1,1)),((0,1),(0,1)),((0,1),(1,0)),((0,1),(1,1)),((1,0),(0,1)),((1,0),(1,1)),((1,1),(0,1)),((1,1),(1,0)),((1,1),(1,1))]
      [((0,0),(0,0)),((0,0),(0,1)),((0,0),(1,0)),((0,0),(1,1)),((0,1),(0,1)),((0,1),(1,0)),((0,1),(1,1)),((1,0),(0,1)),((1,0),(1,1)),((1,1),(0,1)),((1,1),(1,0)),((1,1),(1,1))]
  ]

litmusTest :: (Eq a, Show a) => String -> (forall t. ConcST t a) -> [a] -> [a] -> [a] -> Test
litmusTest name act sq tso pso = testGroup name . hUnitTestToTests $ test
  [ testDejafu' SequentialConsistency defaultBounds act "SQ"  (gives' sq)
  , testDejafu' TotalStoreOrder       defaultBounds act "TSO" (gives' tso)
  , testDejafu' PartialStoreOrder     defaultBounds act "PSO" (gives' pso)
  ]

-------------------------------------------------------------------------------

-- The following collection of litmus tests are all from
-- <https://orbi.ulg.ac.be/bitstream/2268/158670/1/thesis.pdf>

-- | Loads are not reordered with other loads and stores are not
-- reordered with other stores.
intelWP21 :: MonadConc m => m (Int, Int)
intelWP21 = snd <$> litmus2
  (\x y -> writeCRef x 1 >> writeCRef y 1)
  (\x y -> (,) <$> readCRef y <*> readCRef x)

-- | Stores are not reordered with older loads.
intelWP22 :: MonadConc m => m (Int, Int)
intelWP22 = litmus2
  (\x y -> do r1 <- readCRef x; writeCRef y 1; pure r1)
  (\x y -> do r2 <- readCRef y; writeCRef x 1; pure r2)

-- | Loads may be reordered with older stores to different locations.
intelWP23 :: MonadConc m => m (Int, Int)
intelWP23 = litmus2
  (\x y -> writeCRef x 1 >> readCRef y)
  (\x y -> writeCRef y 1 >> readCRef x)

-- | Loads are not reordered with older stores to the same location.
intelWP24 :: MonadConc m => m (Int, Int)
intelWP24 = litmus2
  (\x _ -> writeCRef x 1 >> readCRef x)
  (\_ y -> writeCRef y 1 >> readCRef y)

-- | Intra-processor forwarding is allowed
intelWP25 :: MonadConc m => m ((Int, Int), (Int, Int))
intelWP25 = litmus2
  (\x y -> do writeCRef x 1; r1 <- readCRef x; r2 <- readCRef y; pure (r1, r2))
  (\x y -> do writeCRef y 1; r3 <- readCRef y; r4 <- readCRef x; pure (r3, r4))

-- | Stores are transitively visible.
intelWP26 :: MonadConc m => m (Int, Int, Int)
intelWP26 = do
  x <- newCRef 0
  y <- newCRef 0
  j1 <- spawn (writeCRef x 1)
  j2 <- spawn (do r1 <- readCRef x; writeCRef x 1; pure r1)
  j3 <- spawn (do r2 <- readCRef y; r3 <- readCRef x; pure (r2,r3))
  (\() r1 (r2,r3) -> (r1,r2,r3)) <$> readMVar j1 <*> readMVar j2 <*> readMVar j3

-- | Total order on stores to the same location.
intelWP27 :: MonadConc m => m ((Int, Int), (Int, Int))
intelWP27 = do
  x <- newCRef 0
  j1 <- spawn (writeCRef x 1)
  j2 <- spawn (writeCRef x 2)
  j3 <- spawn (do r1 <- readCRef x; r2 <- readCRef x; pure (r1, r2))
  j4 <- spawn (do r3 <- readCRef x; r4 <- readCRef x; pure (r3, r4))
  (\() () r12 r23 -> (r12, r23)) <$> readMVar j1 <*> readMVar j2 <*> readMVar j3 <*> readMVar j4

-- | Independent Read Independent Write.
--
-- IRIW is a standard litmus test which allows in some architectures
-- ((1,0),(1,0)). Intel (and TSO/PSO) forbid it.
intelWP28 :: MonadConc m => m ((Int, Int), (Int, Int))
intelWP28 = do
  x <- newCRef 0
  y <- newCRef 0
  j1 <- spawn (writeCRef x 1)
  j2 <- spawn (writeCRef y 1)
  j3 <- spawn (do r1 <- readCRef x; r2 <- readCRef y; pure (r1, r2))
  j4 <- spawn (do r3 <- readCRef y; r4 <- readCRef x; pure (r3, r4))
  (\() () r12 r23 -> (r12, r23)) <$> readMVar j1 <*> readMVar j2 <*> readMVar j3 <*> readMVar j4

-------------------------------------------------------------------------------

-- | Create two @CRef@s, fork the two threads, and return the result.
litmus2 :: MonadConc m
  => (CRef m Int -> CRef m Int -> m b)
  -> (CRef m Int -> CRef m Int -> m c)
  -> m (b, c)
litmus2 thread1 thread2 = do
  x <- newCRef 0
  y <- newCRef 0
  j1 <- spawn (thread1 x y)
  j2 <- spawn (thread2 x y)
  (,) <$> readMVar j1 <*> readMVar j2
