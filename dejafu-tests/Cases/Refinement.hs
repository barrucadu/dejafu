module Cases.Refinement where

import Control.Concurrent.Classy.MVar
import Control.Monad (void)
import Test.DejaFu.Conc (ConcIO)
import Test.DejaFu.Refinement
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (test)
import Test.HUnit.DejaFu (testProperty)

import Utils

tests :: [Test]
tests =
  [ testGroup "MVar" . hUnitTestToTests . test $
    [ testProperty "read_idempotent_s"            prop_mvar_read_idempotent_s
    , testProperty "read_idempotent_c"            prop_mvar_read_idempotent_c
    , testProperty "read_neq_take_put"            prop_mvar_read_neq_take_put
    , testProperty "read_sr_take_put"             prop_mvar_read_sr_take_put
    , testProperty "take_eq_read_take_s"          prop_mvar_take_eq_read_take_s
    , testProperty "take_neq_read_take_c"         prop_mvar_take_neq_read_take_c
    , testProperty "take_sr_read_take_c"          prop_mvar_take_sr_read_take_c
    , testProperty "prop_mvar_put_neq_put_read_s" prop_mvar_put_neq_put_read_s
    , testProperty "prop_mvar_put_sr_put_read_s"  prop_mvar_put_sr_put_read_s
    , testProperty "prop_mvar_put_neq_put_read_c" prop_mvar_put_neq_put_read_c
    , testProperty "prop_mvar_put_sr_put_read_c"  prop_mvar_put_sr_put_read_c
    ]
  ]

-------------------------------------------------------------------------------
-- MVars

mvar :: (MVar ConcIO Int -> ConcIO a) ->  Sig (MVar ConcIO Int) (Maybe Int) (Maybe Int)
mvar e = Sig
  { initialise = maybe newEmptyMVar newMVar
  , observe    = const . tryTakeMVar
  , interfere  = \v mi -> tryTakeMVar v >> maybe (pure ()) (void . tryPutMVar v) mi
  , expression = void . e
  }

-- | @readMVar@ is idempotent when composed sequentially.
prop_mvar_read_idempotent_s =
  mvar readMVar === mvar (\v -> readMVar v >> readMVar v)

-- | @readMVar@ is idempotent when composed concurrently.
prop_mvar_read_idempotent_c =
  mvar readMVar === mvar (\v -> readMVar v ||| readMVar v)

-- | @readMVar@ is not equivalent to a take followed by a put.
prop_mvar_read_neq_take_put = expectFailure $
  mvar readMVar === mvar (\v -> takeMVar v >>= putMVar v)

-- | @readMVar@ is a strict refinement of a take followed by a put.
prop_mvar_read_sr_take_put =
  mvar readMVar ->- mvar (\v -> takeMVar v >>= putMVar v)

-- | @takeMVar@ is equivalent to a read followed by a take.
prop_mvar_take_eq_read_take_s =
  mvar takeMVar === mvar (\v -> readMVar v >> takeMVar v)

-- | @takeMVar@ is not equivalent to a read concurrently composed with a take.
prop_mvar_take_neq_read_take_c = expectFailure $
  mvar takeMVar === mvar (\v -> readMVar v ||| takeMVar v)

-- | @takeMVar@ is a strict refinement of a read concurrently composed with a take.
prop_mvar_take_sr_read_take_c =
  mvar takeMVar ->- mvar (\v -> readMVar v ||| takeMVar v)

-- | @putMVar@ is not equivalent to a put followed by a read.
prop_mvar_put_neq_put_read_s x = expectFailure $
  mvar (\v -> putMVar v x) === mvar (\v -> putMVar v x >> readMVar v)

-- | @putMVar@ is a strict refinement of a put followed by a read.
prop_mvar_put_sr_put_read_s x =
  mvar (\v -> putMVar v x) ->- mvar (\v -> putMVar v x >> readMVar v)

-- | @putMVar@ is not equivalent to a put concurrently composed with a read.
prop_mvar_put_neq_put_read_c x = expectFailure $
  mvar (\v -> putMVar v x) === mvar (\v -> putMVar v x ||| readMVar v)

-- | @putMVar@ is a strict refinement of a put concurrently composed with a read.
prop_mvar_put_sr_put_read_c x =
  mvar (\v -> putMVar v x) ->- mvar (\v -> putMVar v x ||| readMVar v)
