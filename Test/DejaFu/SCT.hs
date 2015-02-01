-- | Systematic testing for concurrent computations.
module Test.DejaFu.SCT
 ( runSCT
 , runSCT'
 , runSCTIO
 , runSCTIO'

 -- * Schedule Bounding
 -- | Schedule bounding is a means of cutting down the search space of
 -- schedules, by taking advantage of some intrinsic properties of
 -- schedules: such as the number of pre-emptions (pre-emption
 -- bounding), or the number of deviations from a deterministic
 -- scheduler (delay bounding); and then exploring all schedules
 -- within the bound.
 , sctBounded
 , sctPreBound
 , sctDelayBound
 , sctBoundedIO
 , sctPreBoundIO
 , sctDelayBoundIO

 -- * Utilities
 , preEmpCount
 ) where

import Test.DejaFu.SCT.Internal
import Test.DejaFu.SCT.Bounding
