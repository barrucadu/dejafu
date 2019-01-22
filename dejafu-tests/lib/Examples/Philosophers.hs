-- An implementation of the Dining Philosophers. This is interesting
-- as it show-cases testing a non-terminating program.
module Examples.Philosophers where

import           Control.Monad            (forever, replicateM)
import           Control.Monad.Conc.Class
import           Test.DejaFu

import           Common

tests :: [TestTree]
tests =
    [ testDejafuWay way defaultMemType "deadlocks" deadlocksSometimes test
    , let settings = set lshowAborts True (fromWayAndMemType way defaultMemType)
      in testDejafuWithSettings settings "loops (with aborts present)" abortsSometimes test
    , expectFail $ testDejafuWay way defaultMemType "loops (with aborts hidden)" abortsSometimes test
    ]
  where
    test = basic (philosophers 3)

-- | Shorter execution length bound
way :: Way
way = systematically defaultBounds { boundLength = Just 30 }

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
