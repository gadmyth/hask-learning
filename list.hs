import Data.List

-- splitAt的list版, 就是传递split的idx数组, 在多个地方进行split
splitsAt :: [Int] -> [a] -> [[a]]
splitsAt idxes lst = f [] idxes lst where
         f cum [] lst = lst:cum
         f cum [idx] lst = (fst:snd:cum) where
           (fst, snd) = splitAt idx lst
         f cum idxes lst = f (snd:cum) iidxes fst where
           (fst, snd) = splitAt idx lst
           idx = last idxes
           iidxes = init idxes
