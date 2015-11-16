module Cases.Litmus (tests) where

import Test.DejaFu (MemType(..), gives')
import Test.HUnit (Test, test)
import Test.HUnit.DejaFu (testDejafu')

import Control.Monad.Conc.Class

tests :: Test
tests = test
  [ testDejafu' SequentialConsistency 2 5 iorefRelaxed "iorefRelaxed-SQ"  $ gives' [(True, True), (True, False), (False, True)]
  , testDejafu' TotalStoreOrder       2 5 iorefRelaxed "iorefRelaxed-TSO" $ gives' [(True, True), (True, False), (False, True), (False, False)]
  , testDejafu' PartialStoreOrder     2 5 iorefRelaxed "iorefRelaxed-PSO" $ gives' [(True, True), (True, False), (False, True), (False, False)]
  ]

--------------------------------------------------------------------------------

-- | Relaxed memory test, from Data.IORef
iorefRelaxed :: MonadConc m => m (Bool, Bool)
iorefRelaxed = do
  r1 <- newCRef False
  r2 <- newCRef False
  x  <- spawn $ writeCRef r1 True >> readCRef r2
  y  <- spawn $ writeCRef r2 True >> readCRef r1
  (,) <$> readCVar x <*> readCVar y
