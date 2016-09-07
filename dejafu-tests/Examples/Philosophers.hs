-- An implementation of the Dining Philosophers. This is interesting
-- as it show-cases testing a non-terminating program.
module Examples.Philosophers where

import Control.Monad (replicateM, forever)
import Control.Monad.Conc.Class
import Data.Functor (void)
import System.Random (mkStdGen)
import Test.DejaFu
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (test)
import Test.HUnit.DejaFu

predicates :: [(String, Predicate ())]
predicates = [ ("deadlocks", deadlocksSometimes)
             , ("aborts", abortsSometimes)
              ]

tests :: [Test]
tests =
  [ testGroup "Systematic" . hUnitTestToTests . test $
      testDejafus' defaultMemType bound (philosophers 3) predicates
  , testGroup "Random" . hUnitTestToTests . test $
      testUnsystematicRandom' SequentialConsistency bound 1000 (mkStdGen 0) (philosophers 3) predicates
  , testGroup "PCT" . hUnitTestToTests . test $
      testUnsystematicPCT' SequentialConsistency bound 1000 (mkStdGen 0) (philosophers 3) predicates
  ]

-- | Shorter execution length bound
bound :: Bounds
bound = defaultBounds { boundLength = Just 30 }

--------------------------------------------------------------------------------

-- | Run the Dining Philosophers. Result is irrelevant, we just care
-- about deadlocks.
philosophers :: MonadConc m => Int -> m ()
philosophers n = do
  forks <- replicateM n newEmptyMVar
  let phils = map (\(i,p) -> p i forks) $ zip [0..] $ replicate n philosopher
  cvars <- mapM spawn phils
  mapM_ takeMVar cvars

  where
    philosopher ident forks = forever $ do
      let leftId  = ident
      let rightId = (ident + 1) `mod` length forks
      putMVar (forks !! leftId) ()
      putMVar (forks !! rightId) ()
      -- In the traditional approach, we'd wait for a random time
      -- here, but we want the only source of (important)
      -- nondeterminism to come from the scheduler, which it does, as
      -- pre-emption is effectively a delay.
      takeMVar $ forks !! leftId
      takeMVar $ forks !! rightId
