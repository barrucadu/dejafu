{-# LANGUAGE ImpredicativeTypes #-}

-- | This is a separate module because of the need for
-- ImpredicativeTypes, which breaks things elsewhere in the main
-- SearchParty module.
module Examples.ClassLaws.Impredicative where

import Test.DejaFu (Failure(..), defaultMemType)
import Test.DejaFu.Deterministic (ConcST, Trace)
import qualified Test.DejaFu.Deterministic as D
import Test.DejaFu.SCT (sctBound, defaultBounds)
import Unsafe.Coerce (unsafeCoerce)

sctBound' :: ConcST t a -> [(Either Failure a, Trace D.ThreadId D.ThreadAction D.Lookahead)]
sctBound' = unsafeCoerce $ sctBound defaultMemType defaultBounds
