{-# LANGUAGE CPP #-}

module Cases.Litmus (tests) where

import Test.DejaFu (MemType(..), defaultBounds, gives')
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (test)
import Test.HUnit.DejaFu (testDejafu')

import Control.Monad.Conc.Class

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

tests :: [Test]
tests = hUnitTestToTests $ test
  [ testDejafu' SequentialConsistency defaultBounds iorefRelaxed "iorefRelaxed-SQ"  $
      gives' [(True, True), (True, False), (False, True)]
  , testDejafu' TotalStoreOrder defaultBounds iorefRelaxed "iorefRelaxed-TSO" $
      gives' [(True, True), (True, False), (False, True), (False, False)]
  , testDejafu' PartialStoreOrder defaultBounds iorefRelaxed "iorefRelaxed-PSO" $
      gives' [(True, True), (True, False), (False, True), (False, False)]
  ]

--------------------------------------------------------------------------------

-- | Relaxed memory test, from Data.IORef
iorefRelaxed :: MonadConc m => m (Bool, Bool)
iorefRelaxed = do
  r1 <- newCRef False
  r2 <- newCRef False
  x  <- spawn $ writeCRef r1 True >> readCRef r2
  y  <- spawn $ writeCRef r2 True >> readCRef r1
  (,) <$> readMVar x <*> readMVar y
