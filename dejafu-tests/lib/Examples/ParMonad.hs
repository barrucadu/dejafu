module Examples.ParMonad where

import           Control.Monad.Conc.Class (MonadConc)
import           Control.Monad.IO.Class   (MonadIO)
import           System.Random            (mkStdGen)
import           Test.DejaFu              (deadlocksNever, deadlocksSometimes)

import qualified Examples.ParMonad.Direct as Par

import           Common

tests :: [TestTree]
tests = toTestList
  [ W "random testing exposes a deadlock" parFilter deadlocksSometimes ("randomly", randomly (mkStdGen 0) 150)
  , W "systematic testing does not" parFilter deadlocksNever ("systematically", systematically defaultBounds)
  ]

parFilter :: (MonadConc m, MonadIO m) => m Bool
parFilter = do
    let p x = x `mod` 2 == 0
    let xs = [0..1] :: [Int]
    s <- Par.runParIO $ parfilter p xs
    pure (s == filter p xs)
  where
    parfilter _ []  = pure []
    parfilter f [x] = pure [x | f x]
    parfilter f xs  = do
      let (as, bs) = halve xs
      v1 <- Par.spawn $ parfilter f as
      v2 <- Par.spawn $ parfilter f bs
      left  <- Par.get v1
      right <- Par.get v2
      pure (left ++ right)

    halve xs = splitAt (length xs `div` 2) xs
