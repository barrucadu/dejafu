module Main where

import qualified Criterion.Main       as C
import           Data.Monoid          (mempty)
import qualified Test.Tasty.Options   as T
import qualified Test.Tasty.Providers as T
import qualified Test.Tasty.Runners   as T

import           Util

main :: IO ()
main = C.defaultMain (T.foldTestTree mkBench mempty tests)

-- | Turn a test tree into a list of benchmarks.
mkBench :: T.TreeFold [C.Benchmark]
mkBench = T.trivialFold
  { T.foldSingle = \opts lbl t -> [C.bench lbl (benchTest opts t)]
  , T.foldGroup = \lbl bs -> [C.bgroup lbl bs]
  }

-- | Turn a test into a benchmark.
benchTest :: T.IsTest t => T.OptionSet -> t -> C.Benchmarkable
benchTest opts t = C.nfIO $ do
  res <- T.run opts t (\_ -> pure ())
  pure (show (T.resultOutcome res), T.resultTime res)
