import Test.QuickCheck
import tracer

instance Arbitrary Color where
    arbitrary     = choose ('\32', '\128')
    coarbitrary c = variant (ord c `rem` 4)