module Integration.Refinement where

import           Control.Concurrent.Classy.MVar
import           Control.Monad                  (void)
import           Test.DejaFu.Refinement
import           Test.Tasty.DejaFu              (testProperty)

import           Common                         hiding (testProperty)

tests :: [TestTree]
tests = [ testGroup "MVar" mvarProps ]

-------------------------------------------------------------------------------

mvar :: (MVar ConcIO Int -> ConcIO a) ->  Sig (MVar ConcIO Int) (Maybe Int) (Maybe Int)
mvar e = Sig
  { initialise = maybe newEmptyMVar newMVar
  , observe    = const . tryTakeMVar
  , interfere  = \v mi -> tryTakeMVar v >> maybe (pure ()) (void . tryPutMVar v) mi
  , expression = void . e
  }

mvarProps :: [TestTree]
mvarProps = toTestList
  [ testProperty "readMVar is idempotent when composed sequentially" $
      mvar readMVar === mvar (\v -> readMVar v >> readMVar v)

  , testProperty "readMVar is idempotent when composed concurrently" $
      mvar readMVar === mvar (\v -> readMVar v ||| readMVar v)

  , testProperty "readMVar is not equivalent to a take followed by a put" $
      expectFailure $ mvar readMVar === mvar (\v -> takeMVar v >>= putMVar v)

  , testProperty "readMVar is a strict refinement of a take followed by a put" $
      mvar readMVar ->- mvar (\v -> takeMVar v >>= putMVar v)

  , testProperty "takeMVar is equivalent to a read followed by a take" $
      mvar takeMVar === mvar (\v -> readMVar v >> takeMVar v)

  , testProperty "takeMVar is not equivalent to a read concurrently composed with a take" $
      expectFailure $ mvar takeMVar === mvar (\v -> readMVar v ||| takeMVar v)

  , testProperty "takeMVar is a strict refinement of a read concurrently composed with a take" $
      mvar takeMVar ->- mvar (\v -> readMVar v ||| takeMVar v)

  , testProperty "putMVar is not equivalent to a put followed by a read" $
      \x -> expectFailure $ mvar (\v -> putMVar v x) === mvar (\v -> putMVar v x >> readMVar v)

  , testProperty "putMVar is a strict refinement of a put followed by a read" $
      \x -> mvar (\v -> putMVar v x) ->- mvar (\v -> putMVar v x >> readMVar v)

  , testProperty "putMVar is not equivalent to a put concurrently composed with a read" $
      \x -> expectFailure $ mvar (\v -> putMVar v x) === mvar (\v -> putMVar v x ||| readMVar v)

  , testProperty "putMVar is a strict refinement of a put concurrently composed with a read" $
      \x -> mvar (\v -> putMVar v x) ->- mvar (\v -> putMVar v x ||| readMVar v)
  ]
