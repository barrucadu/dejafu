{-# LANGUAGE ImpredicativeTypes #-}

-- | This is a separate module because of the need for
-- ImpredicativeTypes, which breaks things elsewhere in the main
-- SearchParty module.
module Examples.ClassLaws.Impredicative where

import Control.Monad.ST (ST, runST)
import Unsafe.Coerce (unsafeCoerce)

runST' :: ST t Bool -> Bool
runST' = unsafeCoerce runST
