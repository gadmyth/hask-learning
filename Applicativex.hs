-- |http://www.soi.city.ac.uk/~ross/papers/Applicative.pdf 

import Control.Monad
import Control.Applicative

sequence_m :: [Maybe a] -> Maybe [a]
sequence_m [] = pure []
sequence_m (mc:mcs) = return (:) `ap` c `ap` sequence cs
